{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Sophie.Generators (SomeResult (..)) where

import           Shardagnostic.Network.Block (mkSerialised)

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.HeaderValidation
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.Ledger.Query
import           Shardagnostic.Consensus.Ledger.SupportsMempool

import qualified Sophie.Spec.Ledger.API as SL

import           Shardagnostic.Consensus.Sophie.Eras
import           Shardagnostic.Consensus.Sophie.Ledger
import           Shardagnostic.Consensus.Sophie.Protocol (OptimumCrypto,
                     TOptimumState (..))

import           Generic.Random (genericArbitraryU)
import           Test.QuickCheck hiding (Result)

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Roundtrip (Coherent (..),
                     SomeResult (..), WithVersion (..))

import           Test.Bcc.Ledger.EvieEraGen ()
import           Test.Bcc.Ledger.Aurum.AurumEraGen ()
import           Test.Bcc.Ledger.JenEraGen ()
import           Test.Bcc.Ledger.SophieMA.Serialisation.Generators ()
import           Test.Consensus.Sophie.MockCrypto (CanMock)
import           Test.Sophie.Spec.Ledger.ConcreteCryptoTypes as SL
import           Test.Sophie.Spec.Ledger.Generator.SophieEraGen ()
import           Test.Sophie.Spec.Ledger.Serialisation.EraIndepGenerators
                     (genCoherentBlock)
import           Test.Sophie.Spec.Ledger.Serialisation.Generators ()

{-------------------------------------------------------------------------------
  Generators

  These are generators for roundtrip tests, so the generated values are not
  necessarily valid
-------------------------------------------------------------------------------}

-- | The upstream 'Arbitrary' instance for Sophie blocks does not generate
-- coherent blocks, so neither does this.
instance CanMock era => Arbitrary (SophieBlock era) where
  arbitrary = mkSophieBlock <$> arbitrary

-- | This uses a different upstream generator to ensure the header and block
-- body relate as expected.
instance CanMock era => Arbitrary (Coherent (SophieBlock era)) where
  arbitrary = Coherent . mkSophieBlock <$> genCoherentBlock

instance CanMock era => Arbitrary (Header (SophieBlock era)) where
  arbitrary = getHeader <$> arbitrary

instance SL.Mock c => Arbitrary (SophieHash c) where
  arbitrary = SophieHash <$> arbitrary

instance CanMock era => Arbitrary (GenTx (SophieBlock era)) where
  arbitrary = mkSophieTx <$> arbitrary

instance CanMock era => Arbitrary (GenTxId (SophieBlock era)) where
  arbitrary = SophieTxId <$> arbitrary

instance CanMock era => Arbitrary (SomeSecond BlockQuery (SophieBlock era)) where
  arbitrary = oneof
    [ pure $ SomeSecond GetLedgerTip
    , pure $ SomeSecond GetEpochNo
    , SomeSecond . GetNonMyopicMemberRewards <$> arbitrary
    , pure $ SomeSecond GetCurrentPParams
    , pure $ SomeSecond GetProposedPParamsUpdates
    , pure $ SomeSecond GetStakeDistribution
    , pure $ SomeSecond DebugEpochState
    , (\(SomeSecond q) -> SomeSecond (GetCBOR q)) <$> arbitrary
    , SomeSecond . GetFilteredDelegationsAndRewardAccounts <$> arbitrary
    , pure $ SomeSecond GetGenesisConfig
    , pure $ SomeSecond DebugNewEpochState
    ]

instance CanMock era => Arbitrary (SomeResult (SophieBlock era)) where
  arbitrary = oneof
    [ SomeResult GetLedgerTip <$> arbitrary
    , SomeResult GetEpochNo <$> arbitrary
    , SomeResult <$> (GetNonMyopicMemberRewards <$> arbitrary) <*> arbitrary
    , SomeResult GetCurrentPParams <$> arbitrary
    , SomeResult GetProposedPParamsUpdates <$> arbitrary
    , SomeResult GetStakeDistribution <$> arbitrary
    , SomeResult DebugEpochState <$> arbitrary
    , (\(SomeResult q r) ->
        SomeResult (GetCBOR q) (mkSerialised (encodeSophieResult q) r)) <$>
      arbitrary
    , SomeResult <$> (GetFilteredDelegationsAndRewardAccounts <$> arbitrary) <*> arbitrary
    , SomeResult GetGenesisConfig . compactGenesis <$> arbitrary
    , SomeResult DebugNewEpochState <$> arbitrary
    ]

instance OptimumCrypto c => Arbitrary (NonMyopicMemberRewards c) where
  arbitrary = NonMyopicMemberRewards <$> arbitrary

instance CanMock era => Arbitrary (Point (SophieBlock era)) where
  arbitrary = BlockPoint <$> arbitrary <*> arbitrary

instance OptimumCrypto c => Arbitrary (TOptimumState c) where
  arbitrary = do
      lastSlot <- frequency
        [ (1, return Origin)
        , (5, NotOrigin . SlotNo <$> choose (0, 100))
        ]
      TOptimumState lastSlot <$> arbitrary

instance CanMock era => Arbitrary (SophieTip era) where
  arbitrary = SophieTip
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary SophieTransition where
  arbitrary = SophieTransitionInfo <$> arbitrary

instance CanMock era => Arbitrary (LedgerState (SophieBlock era)) where
  arbitrary = SophieLedgerState
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance CanMock era => Arbitrary (AnnTip (SophieBlock era)) where
  arbitrary = AnnTip
    <$> arbitrary
    <*> (BlockNo <$> arbitrary)
    <*> arbitrary

instance Arbitrary SophieNodeToNodeVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SophieNodeToClientVersion where
  arbitrary = arbitraryBoundedEnum

instance SophieBasedEra era
      => Arbitrary (SomeSecond (NestedCtxt f) (SophieBlock era)) where
  arbitrary = return (SomeSecond indexIsTrivial)

{-------------------------------------------------------------------------------
  Generators for bcc-ledger-specs
-------------------------------------------------------------------------------}

instance Arbitrary (SL.PParams' SL.StrictMaybe era) where
  arbitrary = genericArbitraryU
  shrink    = genericShrink

instance OptimumCrypto c => Arbitrary (SL.ChainDepState c) where
  arbitrary = genericArbitraryU
  shrink = genericShrink

{-------------------------------------------------------------------------------
  Versioned generators for serialisation
-------------------------------------------------------------------------------}

-- | Some 'Query's are only supported by 'SophieNodeToClientVersion2', so we
-- make sure to not generate those queries in combination with
-- 'SophieNodeToClientVersion1'.
instance CanMock era
      => Arbitrary (WithVersion SophieNodeToClientVersion (SomeSecond BlockQuery (SophieBlock era))) where
  arbitrary = do
      query@(SomeSecond q) <- arbitrary
      version <- arbitrary `suchThat` querySupportedVersion q
      return $ WithVersion version query
