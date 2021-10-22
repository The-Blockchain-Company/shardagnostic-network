{-# OPTIONS_GHC -Wno-orphans #-}
module Shardagnostic.Consensus.HardFork.Combinator.Ledger.PeerSelection () where

import           Data.SOP.Strict

import           Shardagnostic.Consensus.Ledger.SupportsPeerSelection

import           Shardagnostic.Consensus.HardFork.Combinator.Abstract
import           Shardagnostic.Consensus.HardFork.Combinator.Basics
import           Shardagnostic.Consensus.HardFork.Combinator.Ledger ()
import qualified Shardagnostic.Consensus.HardFork.Combinator.State as State

instance CanHardFork xs => LedgerSupportsPeerSelection (HardForkBlock xs) where
  getPeers =
        hcollapse
      . hcmap proxySingle (K . getPeers)
      . State.tip
      . hardForkLedgerStatePerEra
