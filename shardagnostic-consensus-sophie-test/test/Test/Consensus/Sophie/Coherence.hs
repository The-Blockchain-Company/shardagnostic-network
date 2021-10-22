{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Sophie.Coherence (tests) where

import           Data.Word (Word32)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits

import           Bcc.Ledger.Aurum.Scripts (ExUnits, pointWiseExUnits)
import           Test.Bcc.Ledger.Aurum.Serialisation.Generators ()

import           Shardagnostic.Consensus.Sophie.Ledger.Mempool (AurumMeasure (..))

tests :: TestTree
tests = testGroup "Sophie coherences" [
      testProperty "TxLimits.<= uses pointWiseExUnits (<=)" leqCoherence
    ]

-- | 'TxLimits.<=' and @'pointWiseExUnits' (<=)@ must agree
leqCoherence :: Word32 -> ExUnits -> ExUnits -> Property
leqCoherence w eu1 eu2 =
    actual === expected
  where
    inj eu = AurumMeasure (TxLimits.ByteSize w) eu

    actual   = inj eu1 TxLimits.<= inj eu2
    expected = pointWiseExUnits (<=) eu1 eu2
