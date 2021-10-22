{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Shardagnostic.Consensus.HardFork.Combinator.Ledger.CommonProtocolParams () where

import           Data.SOP.Strict

import           Shardagnostic.Consensus.Ledger.CommonProtocolParams

import           Shardagnostic.Consensus.HardFork.Combinator.Abstract
import           Shardagnostic.Consensus.HardFork.Combinator.Basics
import           Shardagnostic.Consensus.HardFork.Combinator.Ledger ()
import qualified Shardagnostic.Consensus.HardFork.Combinator.State as State

instance CanHardFork xs => CommonProtocolParams (HardForkBlock xs) where
  maxHeaderSize = askCurrentLedger maxHeaderSize
  maxTxSize     = askCurrentLedger maxTxSize

askCurrentLedger
  :: CanHardFork xs
  => (forall blk. CommonProtocolParams blk => LedgerState blk -> a)
  -> LedgerState (HardForkBlock xs) -> a
askCurrentLedger f =
      hcollapse
    . hcmap proxySingle (K . f)
    . State.tip
    . hardForkLedgerStatePerEra
