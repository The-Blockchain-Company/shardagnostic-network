{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Sophie (
    SophieTxGenExtra (..)
  , WhetherToGeneratePPUs (..)
  , genTx
  , mkGenEnv
  ) where

import           Control.Monad.Except (runExcept)

import           Bcc.Crypto.Hash (HashAlgorithm)

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Config
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.Ledger.SupportsMempool

import qualified Sophie.Spec.Ledger.API as SL

import           Shardagnostic.Consensus.Sophie.Ledger

import           Test.QuickCheck

import           Test.ThreadNet.TxGen (TxGen (..))

import qualified Test.Sophie.Spec.Ledger.Generator.Constants as Gen
import qualified Test.Sophie.Spec.Ledger.Generator.Core as Gen
import           Test.Sophie.Spec.Ledger.Generator.EraGen
                     (EraGen (genEraTwoPhase2Arg, genEraTwoPhase3Arg))
import qualified Test.Sophie.Spec.Ledger.Generator.Presets as Gen.Presets
import           Test.Sophie.Spec.Ledger.Generator.SophieEraGen ()
import qualified Test.Sophie.Spec.Ledger.Generator.Utxo as Gen

import           Test.Consensus.Sophie.MockCrypto (MockCrypto, MockSophie)
import           Test.ThreadNet.Infra.Sophie

data SophieTxGenExtra h = SophieTxGenExtra
  { -- | Generator environment.
    stgeGenEnv  :: Gen.GenEnv (MockSophie h)
    -- | Generate no transactions before this slot.
  , stgeStartAt :: SlotNo
  }

instance HashAlgorithm h => TxGen (SophieBlock (MockSophie h)) where

  type TxGenExtra (SophieBlock (MockSophie h)) = SophieTxGenExtra h

  testGenTxs _coreNodeId _numCoreNodes curSlotNo cfg extra lst
      | stgeStartAt > curSlotNo = pure []

      -- TODO Temporarily disable the transaction generator until we fix the
      -- failing assertion in TxSubmission.Inbound, see #2680.
      --
      -- When fixed, remove the line below to re-enable the transaction
      -- generator.
      | otherwise               = pure []

      | otherwise               = do
      n <- choose (0, 20)
      go [] n $ applyChainTick lcfg curSlotNo lst
    where
      SophieTxGenExtra
        { stgeGenEnv
        , stgeStartAt
        } = extra

      lcfg :: LedgerConfig (SophieBlock (MockSophie h))
      lcfg = configLedger cfg

      go :: [GenTx (SophieBlock (MockSophie h))]  -- ^ Accumulator
         -> Integer  -- ^ Number of txs to still produce
         -> TickedLedgerState (SophieBlock (MockSophie h))
         -> Gen [GenTx (SophieBlock (MockSophie h))]
      go acc 0 _  = return (reverse acc)
      go acc n st = do
        mbTx <- genTx cfg curSlotNo st stgeGenEnv
        case mbTx of
          Nothing -> return (reverse acc)  -- cannot afford more transactions
          Just tx -> case runExcept $ fst <$> applyTx lcfg DoNotIntervene curSlotNo tx st of
              -- We don't mind generating invalid transactions
              Left  _   -> go (tx:acc) (n - 1) st
              Right st' -> go (tx:acc) (n - 1) st'

genTx
  :: forall h. HashAlgorithm h
  => TopLevelConfig (SophieBlock (MockSophie h))
  -> SlotNo
  -> TickedLedgerState (SophieBlock (MockSophie h))
  -> Gen.GenEnv (MockSophie h)
  -> Gen (Maybe (GenTx (SophieBlock (MockSophie h))))
genTx _cfg slotNo TickedSophieLedgerState { tickedSophieLedgerState } genEnv =
    Just . mkSophieTx <$> Gen.genTx
      genEnv
      ledgerEnv
      (utxoSt, dpState)
  where
    epochState :: SL.EpochState (MockSophie h)
    epochState = SL.nesEs tickedSophieLedgerState

    ledgerEnv :: SL.LedgerEnv (MockSophie h)
    ledgerEnv = SL.LedgerEnv {
        ledgerSlotNo   = slotNo
      , ledgerIx       = 0 -- TODO Ix
      , ledgerPp       = SL.esPp epochState
      , ledgerAccount  = SL.esAccountState epochState
      }

    utxoSt :: SL.UTxOState (MockSophie h)
    utxoSt =
        SL._utxoState
      . SL.esLState
      $ epochState

    dpState :: SL.DPState (MockCrypto h)
    dpState =
        SL._delegationState
      . SL.esLState
      $ epochState

data WhetherToGeneratePPUs = DoNotGeneratePPUs | DoGeneratePPUs
  deriving (Show)

mkGenEnv ::
     forall h. HashAlgorithm h
  => WhetherToGeneratePPUs
  -> [CoreNode (MockCrypto h)]
  -> Gen.GenEnv (MockSophie h)
mkGenEnv whetherPPUs coreNodes = Gen.GenEnv keySpace scriptSpace constants
  where
    -- Configuration of the transaction generator
    constants :: Gen.Constants
    constants =
        setCerts $
        setPPUs $
        Gen.defaultConstants
          { Gen.frequencyMIRCert = 0
          , Gen.genTxStableUtxoSize = 100
          , Gen.genTxUtxoIncrement = 3
          }
      where
        -- Testing with certificates requires additional handling in the
        -- testing framework, because, for example, they may transfer block
        -- issuance rights from one node to another, and we must have the
        -- relevant nodes brought online at that point.
        setCerts cs = cs{ Gen.maxCertsPerTx = 0 }

        setPPUs cs = case whetherPPUs of
            DoGeneratePPUs    -> cs
            DoNotGeneratePPUs -> cs{ Gen.frequencyTxUpdates = 0 }

    keySpace :: Gen.KeySpace (MockSophie h)
    keySpace =
      Gen.KeySpace
        (cnkiCoreNode <$> cn)
        ksGenesisDelegates
        ksStakePools
        (ksKeyPairs <> (cnkiKeyPair <$> cn))
        ksMSigScripts
      where
        cn = coreNodeKeys <$> coreNodes
        Gen.KeySpace_
          { ksKeyPairs,
            ksMSigScripts,
            ksGenesisDelegates,
            ksStakePools
          } =
            Gen.Presets.keySpace @(MockSophie h) constants

    scriptSpace :: Gen.ScriptSpace (MockSophie h)
    scriptSpace =
      Gen.Presets.scriptSpace @(MockSophie h)
           (genEraTwoPhase3Arg @(MockSophie h))
           (genEraTwoPhase2Arg @(MockSophie h))
