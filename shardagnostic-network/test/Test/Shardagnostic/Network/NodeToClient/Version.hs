{-# LANGUAGE NamedFieldPuns  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Shardagnostic.Network.NodeToClient.Version (tests) where

import           Shardagnostic.Network.CodecCBORTerm
import           Shardagnostic.Network.Magic
import           Shardagnostic.Network.NodeToClient.Version

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Shardagnostic.Network.NodeToClient.Version"
    [ testProperty "nodeToClientCodecCBORTerm" prop_nodeToClientCodec
    ]

data VersionAndVersionData =
    VersionAndVersionData NodeToClientVersion NodeToClientVersionData
  deriving Show

instance Arbitrary VersionAndVersionData where
    arbitrary =
      VersionAndVersionData
        <$> elements [ NodeToClientV_1, NodeToClientV_2, NodeToClientV_3 ]
        <*> (NodeToClientVersionData . NetworkMagic <$> arbitrary)

prop_nodeToClientCodec :: VersionAndVersionData -> Bool
prop_nodeToClientCodec (VersionAndVersionData vNumber vData) =
      case decodeTerm (encodeTerm vData) of
        Right vData' -> networkMagic vData' == networkMagic vData
        Left {}      -> False
    where
      CodecCBORTerm { encodeTerm, decodeTerm } = nodeToClientCodecCBORTerm vNumber
