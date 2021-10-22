{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Shardagnostic.Consensus.MiniProtocol.LocalStateQuery.Server (localStateQueryServer) where

import           Shardagnostic.Network.Protocol.LocalStateQuery.Server
import           Shardagnostic.Network.Protocol.LocalStateQuery.Type
                     (AcquireFailure (..))

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Ledger.Extended
import           Shardagnostic.Consensus.Ledger.Query
import           Shardagnostic.Consensus.Util.IOLike

localStateQueryServer ::
     forall m blk. (IOLike m, QueryLedger blk, ConfigSupportsNode blk)
  => ExtLedgerCfg blk
  -> STM m (Point blk)
     -- ^ Get tip point
  -> (Point blk -> STM m (Maybe (ExtLedgerState blk)))
     -- ^ Get a past ledger
  -> STM m (Point blk)
     -- ^ Get the immutable point
  -> LocalStateQueryServer blk (Point blk) (Query blk) m ()
localStateQueryServer cfg getTipPoint getPastLedger getImmutablePoint =
    LocalStateQueryServer $ return idle
  where
    idle :: ServerStIdle blk (Point blk) (Query blk) m ()
    idle = ServerStIdle {
          recvMsgAcquire = handleAcquire
        , recvMsgDone    = return ()
        }

    handleAcquire :: Maybe (Point blk)
                  -> m (ServerStAcquiring blk (Point blk) (Query blk) m ())
    handleAcquire mpt = do
        (pt, mPastLedger, immutablePoint) <- atomically $ do
          pt <- maybe getTipPoint pure mpt
          (pt,,) <$> getPastLedger pt <*> getImmutablePoint

        return $ case mPastLedger of
          Just pastLedger
            -> SendMsgAcquired $ acquired pastLedger
          Nothing
            | pointSlot pt < pointSlot immutablePoint
            -> SendMsgFailure AcquireFailurePointTooOld idle
            | otherwise
            -> SendMsgFailure AcquireFailurePointNotOnChain idle

    acquired :: ExtLedgerState blk
             -> ServerStAcquired blk (Point blk) (Query blk) m ()
    acquired ledgerState = ServerStAcquired {
          recvMsgQuery     = handleQuery ledgerState
        , recvMsgReAcquire = handleAcquire
        , recvMsgRelease   = return idle
        }

    handleQuery ::
         ExtLedgerState blk
      -> Query blk result
      -> m (ServerStQuerying blk (Point blk) (Query blk) m () result)
    handleQuery ledgerState query = return $
        SendMsgResult
          (answerQuery cfg query ledgerState)
          (acquired ledgerState)
