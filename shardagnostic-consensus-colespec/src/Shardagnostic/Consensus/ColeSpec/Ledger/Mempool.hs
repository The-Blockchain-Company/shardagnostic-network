{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Shardagnostic.Consensus.ColeSpec.Ledger.Mempool (
    -- * Type family instances
    GenTx (..)
  , Validated (..)
  ) where

import           Codec.Serialise
import           GHC.Generics (Generic)
import           NoThunks.Class (AllowThunk (..), NoThunks)

import           Shardagnostic.Consensus.Ledger.SupportsMempool

import           Shardagnostic.Consensus.ColeSpec.Ledger.Block
import           Shardagnostic.Consensus.ColeSpec.Ledger.GenTx
                     (ColeSpecGenTx (..), ColeSpecGenTxErr (..))
import qualified Shardagnostic.Consensus.ColeSpec.Ledger.GenTx as GenTx
import           Shardagnostic.Consensus.ColeSpec.Ledger.Ledger
import           Shardagnostic.Consensus.ColeSpec.Ledger.Orphans ()

newtype instance GenTx ColeSpecBlock = ColeSpecGenTx {
      unColeSpecGenTx :: ColeSpecGenTx
    }
  deriving stock (Show, Generic)
  deriving anyclass (Serialise)
  deriving NoThunks via AllowThunk (GenTx ColeSpecBlock)

newtype instance Validated (GenTx ColeSpecBlock) = ValidatedColeSpecGenTx {
      forgetValidatedColeSpecGenTx :: GenTx ColeSpecBlock
    }
  deriving stock (Show, Generic)
  deriving anyclass NoThunks

type instance ApplyTxErr ColeSpecBlock = ColeSpecGenTxErr

instance LedgerSupportsMempool ColeSpecBlock where
  applyTx cfg _wti _slot tx (TickedColeSpecLedgerState tip st) =
        fmap (\st' ->
               ( TickedColeSpecLedgerState tip st'
               , ValidatedColeSpecGenTx tx
               )
             )
      $ GenTx.apply cfg (unColeSpecGenTx tx) st

  -- Cole spec doesn't have multiple validation modes
  reapplyTx cfg slot vtx st =
        fmap fst
      $ applyTx cfg DoNotIntervene slot (forgetValidatedColeSpecGenTx vtx) st

  -- Dummy values, as these are not used in practice.
  txsMaxBytes   = const maxBound
  txInBlockSize = const 0

  txForgetValidated = forgetValidatedColeSpecGenTx
