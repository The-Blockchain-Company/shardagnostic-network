{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Cole.Serialisation (tests) where

import           Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Functor.Identity

import           Bcc.Chain.Block (ABlockOrBoundary (..))
import qualified Bcc.Chain.Block as CC.Block
import qualified Bcc.Chain.Update as CC.Update

import           Shardagnostic.Consensus.Config
import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits
import           Shardagnostic.Consensus.Node.ProtocolInfo
import           Shardagnostic.Consensus.Node.Serialisation ()
import           Shardagnostic.Consensus.Util (Dict (..))

import           Shardagnostic.Consensus.Storage.Common (BinaryBlockInfo (..))

import           Shardagnostic.Consensus.Cole.Ledger
import           Shardagnostic.Consensus.Cole.Node

import           Test.QuickCheck hiding (Result)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Test.Bcc.Chain.Genesis.Dummy as CC

import           Test.Util.Corruption
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Roundtrip

import           Test.Consensus.Cole.Generators

tests :: TestTree
tests = testGroup "Cole"
    [ roundtrip_all testCodecCfg dictNestedHdr

    , testProperty "BinaryBlockInfo sanity check" prop_coleBinaryBlockInfo

    , testGroup "Integrity"
        [ testProperty "detect corruption in RegularBlock" prop_detectCorruption_RegularBlock
        ]
    ]
  where
    dictNestedHdr :: forall a. NestedCtxt_ ColeBlock Header a -> Dict (Eq a, Show a)
    dictNestedHdr (CtxtColeBoundary _) = Dict
    dictNestedHdr (CtxtColeRegular  _) = Dict

{-------------------------------------------------------------------------------
  BinaryBlockInfo
-------------------------------------------------------------------------------}

prop_coleBinaryBlockInfo :: ColeBlock -> Property
prop_coleBinaryBlockInfo blk =
    headerAnnotation === extractedHeader
  where
    BinaryBlockInfo { headerOffset, headerSize } =
      coleBinaryBlockInfo blk

    extractedHeader :: Lazy.ByteString
    extractedHeader =
        Lazy.take (fromIntegral headerSize)   $
        Lazy.drop (fromIntegral headerOffset) $
        toLazyByteString (encodeColeBlock blk)

    headerAnnotation :: Lazy.ByteString
    headerAnnotation = Lazy.fromStrict $ case coleBlockRaw blk of
      ABOBBoundary b -> CC.Block.boundaryHeaderAnnotation $ CC.Block.boundaryHeader b
      ABOBBlock    b -> CC.Block.headerAnnotation         $ CC.Block.blockHeader    b

{-------------------------------------------------------------------------------
  Integrity
-------------------------------------------------------------------------------}

-- | Test that we can detect random bitflips in blocks.
--
-- We cannot do this for EBBs, as they are not signed nor have a hash, so we
-- only test with regular blocks.
prop_detectCorruption_RegularBlock :: RegularBlock -> Corruption -> Property
prop_detectCorruption_RegularBlock (RegularBlock blk) =
    detectCorruption
      encodeColeBlock
      (decodeColeBlock epochSlots)
      (verifyBlockIntegrity (configBlock testCfg))
      blk

-- | Matches the values used for the generators.
testCfg :: TopLevelConfig ColeBlock
testCfg = pInfoConfig protocolInfo
  where
    protocolInfo :: ProtocolInfo Identity ColeBlock
    protocolInfo =
      protocolInfoCole $ ProtocolParamsCole {
          coleGenesis                = CC.dummyConfig
        , colePbftSignatureThreshold = Just (PBftSignatureThreshold 0.5)
        , coleProtocolVersion        = CC.Update.ProtocolVersion 1 0 0
        , coleSoftwareVersion        = CC.Update.SoftwareVersion (CC.Update.ApplicationName "Bcc Test") 2
        , coleLeaderCredentials      = Nothing
        , coleMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }

-- | Matches the values used for the generators.
testCodecCfg :: CodecConfig ColeBlock
testCodecCfg = configCodec testCfg
