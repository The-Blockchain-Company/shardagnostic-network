{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Bcc.Golden (tests) where

import           System.FilePath ((</>))

import           Shardagnostic.Consensus.HardFork.Combinator.Serialisation
import           Shardagnostic.Consensus.Ledger.Query (QueryVersion)

import           Shardagnostic.Consensus.Bcc.Block
import           Shardagnostic.Consensus.Bcc.Node

import           Test.Tasty

import           Test.Util.Paths
import           Test.Util.Serialisation.Golden

import           Test.Consensus.Bcc.Examples

tests :: TestTree
tests = goldenTest_all codecConfig $(getGoldenDir) examples

instance BccHardForkConstraints c
      => ToGoldenDirectory (HardForkNodeToNodeVersion (BccEras c)) where
  toGoldenDirectory v = case v of
    BccNodeToNodeVersion1 -> "BccNodeToNodeVersion1"
    BccNodeToNodeVersion2 -> "BccNodeToNodeVersion2"
    BccNodeToNodeVersion3 -> "BccNodeToNodeVersion3"
    BccNodeToNodeVersion4 -> "BccNodeToNodeVersion4"
    BccNodeToNodeVersion5 -> "BccNodeToNodeVersion5"
    _                         -> error $ "Unknown version: " <> show v

instance BccHardForkConstraints c
      => ToGoldenDirectory (QueryVersion, HardForkNodeToClientVersion (BccEras c)) where
  toGoldenDirectory (queryVersion, blockVersion) = show queryVersion </> case blockVersion of
    BccNodeToClientVersion1 -> "BccNodeToClientVersion1"
    BccNodeToClientVersion2 -> "BccNodeToClientVersion2"
    BccNodeToClientVersion3 -> "BccNodeToClientVersion3"
    BccNodeToClientVersion4 -> "BccNodeToClientVersion4"
    BccNodeToClientVersion5 -> "BccNodeToClientVersion5"
    BccNodeToClientVersion6 -> "BccNodeToClientVersion6"
    BccNodeToClientVersion7 -> "BccNodeToClientVersion7"
    _                           -> error $ "Unknown version: " <> show blockVersion
