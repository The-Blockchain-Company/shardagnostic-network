{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}

module Test.Consensus.Sophie.Examples (
    -- * Setup
    codecConfig
  , testSophieGenesis
    -- * Examples
  , examplesEvie
  , examplesAurum
  , examplesJen
  , examplesSophie
  ) where

import qualified Data.Set as Set

import           Shardagnostic.Network.Block (Serialised (..))

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.HeaderValidation
import           Shardagnostic.Consensus.Ledger.Extended
import           Shardagnostic.Consensus.Ledger.SupportsMempool
import           Shardagnostic.Consensus.Storage.Serialisation

import           Test.Sophie.Spec.Ledger.Orphans ()

import           Shardagnostic.Consensus.Sophie.Eras
import           Shardagnostic.Consensus.Sophie.Ledger

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Golden (labelled, unlabelled)
import qualified Test.Util.Serialisation.Golden as Golden
import           Test.Util.Serialisation.Roundtrip (SomeResult (..))

import           Shardagnostic.Consensus.Sophie.Protocol
                     (TOptimumState (TOptimumState))

import           Test.Bcc.Ledger.Evie.Examples.Consensus
                     (ledgerExamplesEvie)
import           Test.Bcc.Ledger.Aurum.Examples.Consensus
                     (ledgerExamplesAurum)
import           Test.Bcc.Ledger.Jen.Examples.Consensus
                     (ledgerExamplesJen)
import           Test.Sophie.Spec.Ledger.Examples.Consensus
                     (SophieLedgerExamples (..), SophieResultExamples (..),
                     ledgerExamplesSophie, testSophieGenesis)

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

codecConfig :: CodecConfig (SophieBlock Shardagnostic.Consensus.Sophie.Eras.StandardSophie)
codecConfig = SophieCodecConfig

fromSophieLedgerExamples
  :: SophieBasedEra era
  => SophieLedgerExamples era
  -> Golden.Examples (SophieBlock era)
fromSophieLedgerExamples SophieLedgerExamples {
                            sleResultExamples = SophieResultExamples{..}
                            , ..} =
  Golden.Examples {
      exampleBlock            = unlabelled blk
    , exampleSerialisedBlock  = unlabelled serialisedBlock
    , exampleHeader           = unlabelled $ getHeader blk
    , exampleSerialisedHeader = unlabelled serialisedHeader
    , exampleHeaderHash       = unlabelled hash
    , exampleGenTx            = unlabelled tx
    , exampleGenTxId          = unlabelled $ txId tx
    , exampleApplyTxErr       = unlabelled sleApplyTxError
    , exampleQuery            = queries
    , exampleResult           = results
    , exampleAnnTip           = unlabelled annTip
    , exampleLedgerState      = unlabelled ledgerState
    , exampleChainDepState    = unlabelled chainDepState
    , exampleExtLedgerState   = unlabelled extLedgerState
    }
  where
    blk = mkSophieBlock sleBlock
    hash = SophieHash sleHashHeader
    serialisedBlock = Serialised "<BLOCK>"
    tx = mkSophieTx sleTx
    serialisedHeader =
      SerialisedHeaderFromDepPair $ GenDepPair (NestedCtxt CtxtSophie) (Serialised "<HEADER>")
    queries = labelled [
          ("GetLedgerTip",              SomeSecond GetLedgerTip)
        , ("GetEpochNo",                SomeSecond GetEpochNo)
        , ("GetCurrentPParams",         SomeSecond GetCurrentPParams)
        , ("GetProposedPParamsUpdates", SomeSecond GetProposedPParamsUpdates)
        , ("GetStakeDistribution",      SomeSecond GetStakeDistribution)
        , ("GetNonMyopicMemberRewards", SomeSecond $ GetNonMyopicMemberRewards sleRewardsCredentials)
        , ("GetGenesisConfig",          SomeSecond GetGenesisConfig)
      ]
    results = labelled [
          ("LedgerTip",              SomeResult GetLedgerTip (blockPoint blk))
        , ("EpochNo",                SomeResult GetEpochNo 10)
        , ("EmptyPParams",           SomeResult GetCurrentPParams srePParams)
        , ("ProposedPParamsUpdates", SomeResult GetProposedPParamsUpdates sreProposedPPUpdates)
        , ("StakeDistribution",      SomeResult GetStakeDistribution srePoolDistr)
        , ("NonMyopicMemberRewards", SomeResult (GetNonMyopicMemberRewards Set.empty)
                                     (NonMyopicMemberRewards $ sreNonMyopicRewards))
        , ("GenesisConfig",          SomeResult GetGenesisConfig (compactGenesis sreSophieGenesis))
        ]
    annTip = AnnTip {
        annTipSlotNo  = SlotNo 14
      , annTipBlockNo = BlockNo 6
      , annTipInfo    = hash
      }
    ledgerState = SophieLedgerState {
        sophieLedgerTip        = NotOrigin SophieTip {
                                    sophieTipSlotNo  = SlotNo 9
                                  , sophieTipBlockNo = BlockNo 3
                                  , sophieTipHash    = hash
                                  }
    , sophieLedgerState      = sleNewEpochState
    , sophieLedgerTransition = SophieTransitionInfo {sophieAfterVoting = 0}
    }
    chainDepState = TOptimumState (NotOrigin 1) sleChainDepState
    extLedgerState = ExtLedgerState
                       ledgerState
                       (genesisHeaderState chainDepState)

examplesSophie :: Golden.Examples (SophieBlock StandardSophie)
examplesSophie = fromSophieLedgerExamples ledgerExamplesSophie

examplesEvie :: Golden.Examples (SophieBlock StandardEvie)
examplesEvie = fromSophieLedgerExamples ledgerExamplesEvie

examplesJen :: Golden.Examples (SophieBlock StandardJen)
examplesJen = fromSophieLedgerExamples ledgerExamplesJen

examplesAurum :: Golden.Examples (SophieBlock StandardAurum)
examplesAurum = fromSophieLedgerExamples ledgerExamplesAurum
