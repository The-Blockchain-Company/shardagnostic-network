{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Shardagnostic.Consensus.MiniProtocol.LocalTxSubmission.Server (
    localTxSubmissionServer
    -- * Trace events
  , TraceLocalTxSubmissionServerEvent (..)
  ) where

import           Control.Tracer

import           Shardagnostic.Network.Protocol.LocalTxSubmission.Server
import           Shardagnostic.Network.Protocol.LocalTxSubmission.Type

import           Shardagnostic.Consensus.Ledger.SupportsMempool
import           Shardagnostic.Consensus.Mempool.API
import           Shardagnostic.Consensus.Util.IOLike


-- | Local transaction submission server, for adding txs to the 'Mempool'
--
localTxSubmissionServer
  :: MonadSTM m
  => Tracer m (TraceLocalTxSubmissionServerEvent blk)
  -> Mempool m blk idx
  -> LocalTxSubmissionServer (GenTx blk) (ApplyTxErr blk) m ()
localTxSubmissionServer tracer mempool =
    server
  where
    server = LocalTxSubmissionServer {
      recvMsgSubmitTx = \tx -> do
        traceWith tracer $ TraceReceivedTx tx
        res <- addLocalTxs mempool [tx]
        case res of
          [addTxRes] -> case addTxRes of
            MempoolTxAdded _tx             -> return (SubmitSuccess, server)
            MempoolTxRejected _tx addTxErr -> return (SubmitFail addTxErr, server)
          -- The output list of addTxs has the same length as the input list.
          _                 -> error "addTxs: unexpected result"

    , recvMsgDone = ()
    }


{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

data TraceLocalTxSubmissionServerEvent blk
  = TraceReceivedTx (GenTx blk)
    -- ^ A transaction was received.

deriving instance Eq   (GenTx blk)
               => Eq   (TraceLocalTxSubmissionServerEvent blk)
deriving instance Show (GenTx blk)
               => Show (TraceLocalTxSubmissionServerEvent blk)
