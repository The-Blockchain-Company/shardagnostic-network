{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Monadic side of the Mempool implementation.
--
-- Using the functions defined in Shardagnostic.Consensus.Mempool.Impl.Pure,
-- a dedicated constructor 'openMempool' is provided to encapsulate the mempool
-- functionality.
--
-- The implementation is based on a MempoolEnv that captures the relevant
-- variables to manage the mempool and is then used to craft functions that
-- conform to the Mempool datatype API.
--
-- The operations performed on the Mempool are written in a pure fashion in
-- Shardagnostic.Consensus.Mempool.Impl.Pure.
module Shardagnostic.Consensus.Mempool.Impl (
    openMempool
    -- * For testing purposes
  , LedgerInterface (..)
  , chainDBLedgerInterface
  , openMempoolWithoutSyncThread
  ) where

import           Control.Monad.Except
import           Data.Typeable

import           Control.Tracer

import           Shardagnostic.Consensus.Storage.ChainDB (ChainDB)
import qualified Shardagnostic.Consensus.Storage.ChainDB.API as ChainDB

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.HeaderValidation
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.Ledger.Extended
import           Shardagnostic.Consensus.Ledger.SupportsMempool
import           Shardagnostic.Consensus.Mempool.API
import           Shardagnostic.Consensus.Mempool.Impl.Pure
import           Shardagnostic.Consensus.Mempool.Impl.Types
import           Shardagnostic.Consensus.Mempool.TxSeq (TicketNo, zeroTicketNo)
import           Shardagnostic.Consensus.Util (whenJust)
import           Shardagnostic.Consensus.Util.IOLike
import           Shardagnostic.Consensus.Util.ResourceRegistry
import           Shardagnostic.Consensus.Util.STM (Watcher (..), forkLinkedWatcher)

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

-- | Create a @Mempool m blk TicketNo@ in @m@ to manipulate the mempool. It
-- will also fork a thread that syncs the mempool and the ledger when the ledger
-- changes.
openMempool
  :: ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => ResourceRegistry m
  -> LedgerInterface m blk
  -> LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> Tracer m (TraceEventMempool blk)
  -> (GenTx blk -> TxSizeInBytes)
  -> m (Mempool m blk TicketNo)
openMempool registry ledger cfg capacityOverride tracer txSize = do
    env <- initMempoolEnv ledger cfg capacityOverride tracer txSize
    forkSyncStateOnTipPointChange registry env
    return $ mkMempool env

-- | Unlike 'openMempool', this function does not fork a background thread
-- that synchronises with the ledger state whenever the later changes.
--
-- Intended for testing purposes.
openMempoolWithoutSyncThread
  :: ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => LedgerInterface m blk
  -> LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> Tracer m (TraceEventMempool blk)
  -> (GenTx blk -> TxSizeInBytes)
  -> m (Mempool m blk TicketNo)
openMempoolWithoutSyncThread ledger cfg capacityOverride tracer txSize =
    mkMempool <$> initMempoolEnv ledger cfg capacityOverride tracer txSize

mkMempool
  :: ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => MempoolEnv m blk -> Mempool m blk TicketNo
mkMempool mpEnv = Mempool
    { tryAddTxs      = implTryAddTxs istate cfg txSize trcr
    , removeTxs      = \txs -> do
        mTrace <- atomically $ do
          is <- readTVar istate
          ls <- getCurrentLedgerState ldgr
          let p = pureRemoveTxs cfg co txs is ls
          runRemoveTxs istate p
        whenJust mTrace (traceWith trcr)
    , syncWithLedger = do
        (mTrace, mp) <- atomically $ do
          is <- readTVar istate
          ls <- getCurrentLedgerState ldgr
          let p = pureSyncWithLedger is ls cfg co
          runSyncWithLedger istate p
        whenJust mTrace (traceWith trcr)
        return mp
    , getSnapshot    = implSnapshotFromIS <$> readTVar istate
    , getSnapshotFor = \fls -> pureGetSnapshotFor cfg fls co <$> readTVar istate
    , getCapacity    = isCapacity <$> readTVar istate
    , getTxSize      = txSize
    , zeroIdx        = zeroTicketNo
    }
   where MempoolEnv{ mpEnvStateVar = istate
                   , mpEnvLedgerCfg = cfg
                   , mpEnvTxSize = txSize
                   , mpEnvTracer = trcr
                   , mpEnvLedger = ldgr
                   , mpEnvCapacityOverride = co
                   } = mpEnv

-- | Abstract interface needed to run a Mempool.
data LedgerInterface m blk = LedgerInterface
    { getCurrentLedgerState :: STM m (LedgerState blk)
    }

-- | Create a 'LedgerInterface' from a 'ChainDB'.
chainDBLedgerInterface ::
     (IOLike m, IsLedger (LedgerState blk))
  => ChainDB m blk -> LedgerInterface m blk
chainDBLedgerInterface chainDB = LedgerInterface
    { getCurrentLedgerState = ledgerState <$> ChainDB.getCurrentLedger chainDB
    }

{-------------------------------------------------------------------------------
  Mempool environment
-------------------------------------------------------------------------------}

-- | The mempool environment captures all the associated variables wrt the
-- Mempool and is accessed by the Mempool interface on demand to perform the
-- different operations.
data MempoolEnv m blk = MempoolEnv {
      mpEnvLedger           :: LedgerInterface m blk
    , mpEnvLedgerCfg        :: LedgerConfig blk
    , mpEnvStateVar         :: StrictTVar m (InternalState blk)
    , mpEnvTracer           :: Tracer m (TraceEventMempool blk)
    , mpEnvTxSize           :: GenTx blk -> TxSizeInBytes
    , mpEnvCapacityOverride :: MempoolCapacityBytesOverride
    }

initMempoolEnv :: ( IOLike m
                  , NoThunks (GenTxId blk)
                  , LedgerSupportsMempool blk
                  , ValidateEnvelope blk
                  )
               => LedgerInterface m blk
               -> LedgerConfig blk
               -> MempoolCapacityBytesOverride
               -> Tracer m (TraceEventMempool blk)
               -> (GenTx blk -> TxSizeInBytes)
               -> m (MempoolEnv m blk)
initMempoolEnv ledgerInterface cfg capacityOverride tracer txSize = do
    st <- atomically $ getCurrentLedgerState ledgerInterface
    let (slot, st') = tickLedgerState cfg (ForgeInUnknownSlot st)
    isVar <- newTVarIO $ initInternalState capacityOverride zeroTicketNo slot st'
    return MempoolEnv
      { mpEnvLedger           = ledgerInterface
      , mpEnvLedgerCfg        = cfg
      , mpEnvStateVar         = isVar
      , mpEnvTracer           = tracer
      , mpEnvTxSize           = txSize
      , mpEnvCapacityOverride = capacityOverride
      }

-- | Spawn a thread which syncs the 'Mempool' state whenever the 'LedgerState'
-- changes.
forkSyncStateOnTipPointChange :: forall m blk. (
                                   IOLike m
                                 , LedgerSupportsMempool blk
                                 , HasTxId (GenTx blk)
                                 , ValidateEnvelope blk
                                 )
                              => ResourceRegistry m
                              -> MempoolEnv m blk
                              -> m ()
forkSyncStateOnTipPointChange registry menv =
    void $ forkLinkedWatcher
      registry
      "Mempool.syncStateOnTipPointChange"
      Watcher {
          wFingerprint = id
        , wInitial     = Nothing
        , wNotify      = action
        , wReader      = getCurrentTip
        }
  where
    MempoolEnv { mpEnvStateVar = istate
               , mpEnvLedger = ldgr
               , mpEnvTracer = trcr
               , mpEnvLedgerCfg = cfg
               , mpEnvCapacityOverride = co
               } = menv

    action :: Point blk -> m ()
    action _tipPoint =
      void $ do
        (mTrace, _) <- atomically $ do
          is <- readTVar istate
          ls <- getCurrentLedgerState ldgr
          let p = pureSyncWithLedger is ls cfg co
          runSyncWithLedger istate p
        whenJust mTrace (traceWith trcr)

    -- Using the tip ('Point') allows for quicker equality checks
    getCurrentTip :: STM m (Point blk)
    getCurrentTip =
          ledgerTipPoint (Proxy @blk)
      <$> getCurrentLedgerState (mpEnvLedger menv)
