{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Sophie.MockCrypto (
    Block
  , CanMock
  , MockCrypto
  , MockSophie
  ) where

import           Test.QuickCheck (Arbitrary)

import           Bcc.Crypto.DSIGN (MockDSIGN)
import           Bcc.Crypto.Hash (HashAlgorithm)
import           Bcc.Crypto.KES (MockKES)

import qualified Bcc.Ledger.Core as Core
import           Bcc.Ledger.Crypto (Crypto (..))
import           Control.State.Transition.Extended (PredicateFailure)
import qualified Sophie.Spec.Ledger.API as SL
import qualified Sophie.Spec.Ledger.Tx as SL (ValidateScript)

import           Test.Bcc.Crypto.VRF.Fake (FakeVRF)
import qualified Test.Sophie.Spec.Ledger.ConcreteCryptoTypes as SL (Mock)
import qualified Test.Sophie.Spec.Ledger.Generator.EraGen as SL (EraGen)

import           Shardagnostic.Consensus.Sophie.Eras (EraCrypto, SophieBasedEra,
                     SophieEra)
import           Shardagnostic.Consensus.Sophie.Ledger (SophieBlock)

-- | A mock replacement for 'StandardCrypto'
--
-- We run the tests with this mock crypto, as it is easier to generate and
-- debug things. The code is parametric in the crypto, so it shouldn't make
-- much of a difference. This also has the important advantage
-- that we can reuse the generators from bcc-ledger-specs.
data MockCrypto h

instance HashAlgorithm h => Crypto (MockCrypto h) where
  type ADDRHASH (MockCrypto h) = h
  type DSIGN    (MockCrypto h) = MockDSIGN
  type HASH     (MockCrypto h) = h
  type KES      (MockCrypto h) = MockKES 10
  type VRF      (MockCrypto h) = FakeVRF

type MockSophie h = SophieEra (MockCrypto h)

instance HashAlgorithm h => SL.OptimumCrypto (MockCrypto h)

type Block h = SophieBlock (MockSophie h)

-- | Cryptography that can easily be mocked
type CanMock era =
  ( SophieBasedEra era
  , SL.EraGen era
  , SL.Mock (EraCrypto era)
  , SL.ValidateScript era
  , Arbitrary (Core.AuxiliaryData era)
  , Arbitrary (Core.PParams era)
  , Arbitrary (Core.Script era)
  , Arbitrary (Core.TxBody era)
  , Arbitrary (Core.Tx era)
  , Arbitrary (Core.TxOut era)
  , Arbitrary (Core.Value era)
  , Arbitrary (PredicateFailure (SL.UTXOW era))
  , Arbitrary (Core.Witnesses era)
  )
