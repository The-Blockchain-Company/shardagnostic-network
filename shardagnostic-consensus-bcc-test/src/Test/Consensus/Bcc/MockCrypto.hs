{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Bcc.MockCrypto (MockCryptoCompatCole) where

import           Bcc.Crypto.DSIGN (Ed25519DSIGN)
import           Bcc.Crypto.Hash (Blake2b_224, Blake2b_256)
import           Bcc.Crypto.KES (MockKES)

import           Bcc.Ledger.Crypto (Crypto (..))
import           Test.Bcc.Crypto.VRF.Fake (FakeVRF)

import           Shardagnostic.Consensus.Sophie.Protocol (OptimumCrypto)

-- | A replacement for 'Test.Consensus.Sophie.MockCrypto' that is compatible
-- with bootstrapping from Cole.
--
-- * The "Shardagnostic.Consensus.Bcc.CanHardFork" translation requires that
--   @ADDRHASH@ has the same bit size as Cole address hashes (ie 224); that's why
--   we use 'Blake2b_224' here.
--
-- * Similarly, @HASH@ has to have the same bit size as Cole header hashes (ie
--   256), that's why we use 'Blake2b_256' here.
--
-- * The @sophie-spec-ledger@ package currently requires that @'DSIGN' ~
--   'Ed25519DSIGN' in order to use Cole bootstrap witnesses.
--
-- * We can still use mock KES and mock VRF.
--
-- Note that many Sophie generators are not instantiated to 'MockSophie' but
-- are constrained by @'CanMock' era@. @'SophieEra' 'MockCryptoCompatCole'@
-- satisfies this constraint, allowing us to reuse these generators for Bcc.
data MockCryptoCompatCole

instance Crypto MockCryptoCompatCole where
  type ADDRHASH MockCryptoCompatCole = Blake2b_224
  type DSIGN    MockCryptoCompatCole = Ed25519DSIGN
  type HASH     MockCryptoCompatCole = Blake2b_256
  type KES      MockCryptoCompatCole = MockKES 10
  type VRF      MockCryptoCompatCole = FakeVRF

instance OptimumCrypto MockCryptoCompatCole
