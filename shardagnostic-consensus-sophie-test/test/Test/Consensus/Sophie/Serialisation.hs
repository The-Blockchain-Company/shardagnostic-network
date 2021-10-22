{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Consensus.Sophie.Serialisation (tests) where

import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString.Lazy as Lazy
import           Data.Proxy (Proxy (..))
import           Data.Word (Word64)

import           Bcc.Crypto.Hash (ShortHash)

import           Shardagnostic.Consensus.Storage.Common (BinaryBlockInfo (..))
import           Shardagnostic.Consensus.Util (Dict (..))

import           Shardagnostic.Consensus.Sophie.Ledger
import           Shardagnostic.Consensus.Sophie.Node ()
import           Shardagnostic.Consensus.Sophie.Node.Serialisation ()

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Test.Util.Corruption
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Roundtrip

import           Test.Consensus.Sophie.Generators ()
import           Test.Consensus.Sophie.MockCrypto

tests :: TestTree
tests = testGroup "Sophie"
    [ roundtrip_all testCodecCfg dictNestedHdr

      -- Test for real crypto too
    , testProperty "hashSize real crypto"       $ prop_hashSize pReal
    , testProperty "ConvertRawHash real crypto" $ roundtrip_ConvertRawHash pReal

    , testProperty "BinaryBlockInfo sanity check" prop_sophieBinaryBlockInfo

    , testGroup "Integrity"
        [ testProperty "generate non-corrupt blocks"  prop_blockIntegrity
        , testProperty "generate non-corrupt headers" prop_headerIntegrity
        , testProperty "detect corruption in blocks"  prop_detectCorruption_Block
        , testProperty "detect corruption in headers" prop_detectCorruption_Header
        ]
    ]
  where
    pReal :: Proxy (SophieBlock (MockSophie ShortHash))
    pReal = Proxy

    testCodecCfg :: CodecConfig (SophieBlock (MockSophie ShortHash))
    testCodecCfg = SophieCodecConfig

    dictNestedHdr ::
         forall a era. SophieBasedEra era
      => NestedCtxt_ (SophieBlock era) Header a -> Dict (Eq a, Show a)
    dictNestedHdr CtxtSophie = Dict

{-------------------------------------------------------------------------------
  BinaryBlockInfo
-------------------------------------------------------------------------------}

prop_sophieBinaryBlockInfo :: Block ShortHash -> Property
prop_sophieBinaryBlockInfo blk =
    encodedHeader === extractedHeader
  where
    BinaryBlockInfo { headerOffset, headerSize } =
      sophieBinaryBlockInfo blk

    extractedHeader :: Lazy.ByteString
    extractedHeader =
        Lazy.take (fromIntegral headerSize)   $
        Lazy.drop (fromIntegral headerOffset) $
        CBOR.toLazyByteString (encodeSophieBlock blk)

    encodedHeader :: Lazy.ByteString
    encodedHeader = CBOR.toLazyByteString $ encodeSophieHeader (getHeader blk)

{-------------------------------------------------------------------------------
  Integrity
-------------------------------------------------------------------------------}

-- TODO test with real crypto

testTOptimumSlotsPerKESPeriod :: Word64
testTOptimumSlotsPerKESPeriod = maxBound

-- | Test that the block we generate pass the 'verifyBlockIntegrity' check
prop_blockIntegrity :: Coherent (Block ShortHash) -> Bool
prop_blockIntegrity =
    verifyBlockIntegrity testTOptimumSlotsPerKESPeriod . getCoherent

-- | Test that the block we generate pass the 'verifyHeaderIntegrity' check
prop_headerIntegrity :: Header (Block ShortHash) -> Bool
prop_headerIntegrity = verifyHeaderIntegrity testTOptimumSlotsPerKESPeriod

-- | Test that we can detect random bitflips in blocks.
prop_detectCorruption_Block :: Coherent (Block ShortHash) -> Corruption -> Property
prop_detectCorruption_Block (Coherent blk) =
    detectCorruption
      encodeSophieBlock
      decodeSophieBlock
      (verifyBlockIntegrity testTOptimumSlotsPerKESPeriod)
      blk

-- | Test that we can detect random bitflips in blocks.
prop_detectCorruption_Header :: Header (Block ShortHash) -> Corruption -> Property
prop_detectCorruption_Header =
    detectCorruption
      encodeSophieHeader
      decodeSophieHeader
      (verifyHeaderIntegrity testTOptimumSlotsPerKESPeriod)
