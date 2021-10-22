{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Test.Consensus.Bcc.Serialisation (tests) where

import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString.Lazy as Lazy

import           Shardagnostic.Network.Block (Serialised (..))

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.HardFork.Combinator.Block
import           Shardagnostic.Consensus.Storage.Serialisation
import           Shardagnostic.Consensus.Util (Dict (..))

import           Shardagnostic.Consensus.Cole.Ledger
import           Shardagnostic.Consensus.Cole.Node ()

import           Shardagnostic.Consensus.Sophie.Ledger
import           Shardagnostic.Consensus.Sophie.Node ()

import           Shardagnostic.Consensus.Bcc.Block
import           Shardagnostic.Consensus.Bcc.Node ()

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Roundtrip

import           Test.Consensus.Bcc.Generators (epochSlots)
import           Test.Consensus.Bcc.MockCrypto (MockCryptoCompatCole)

tests :: TestTree
tests = testGroup "Bcc"
    [ roundtrip_all testCodecCfg dictNestedHdr
    , testProperty "BinaryBlockInfo sanity check" prop_BccBinaryBlockInfo
    ]

testCodecCfg :: BccCodecConfig MockCryptoCompatCole
testCodecCfg =
  BccCodecConfig
    (ColeCodecConfig epochSlots)
    SophieCodecConfig
    SophieCodecConfig
    SophieCodecConfig
    SophieCodecConfig

dictNestedHdr
  :: forall a.
     NestedCtxt_ (BccBlock MockCryptoCompatCole) Header a
  -> Dict (Eq a, Show a)
dictNestedHdr = \case
    NCZ (CtxtColeBoundary {})              -> Dict
    NCZ (CtxtColeRegular  {})              -> Dict
    NCS (NCZ CtxtSophie)                   -> Dict
    NCS (NCS (NCZ CtxtSophie))             -> Dict
    NCS (NCS (NCS (NCZ CtxtSophie)))       -> Dict
    NCS (NCS (NCS (NCS (NCZ CtxtSophie)))) -> Dict

{-------------------------------------------------------------------------------
  BinaryBlockInfo
-------------------------------------------------------------------------------}

prop_BccBinaryBlockInfo :: BccBlock MockCryptoCompatCole -> Property
prop_BccBinaryBlockInfo blk =
    encodedNestedHeader === extractedHeader
  where
    BinaryBlockInfo { headerOffset, headerSize } =
      getBinaryBlockInfo blk

    extractedHeader :: Lazy.ByteString
    extractedHeader =
        Lazy.take (fromIntegral headerSize)   $
        Lazy.drop (fromIntegral headerOffset) $
        CBOR.toLazyByteString (encodeDisk testCodecCfg blk)

    encodedNestedHeader :: Lazy.ByteString
    encodedNestedHeader = case encodeDepPair testCodecCfg (unnest (getHeader blk)) of
        GenDepPair _ (Serialised bytes) -> bytes
