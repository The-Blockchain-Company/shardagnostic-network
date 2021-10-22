{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Sophie.Golden (tests) where

import           System.FilePath ((</>))

import           Shardagnostic.Consensus.Ledger.Query (QueryVersion)

import           Shardagnostic.Consensus.Sophie.Ledger.NetworkProtocolVersion
import           Shardagnostic.Consensus.Sophie.Node ()

import           Test.Tasty

import           Test.Util.Paths
import           Test.Util.Serialisation.Golden

import           Test.Consensus.Sophie.Examples

tests :: TestTree
tests = goldenTest_all codecConfig $(getGoldenDir) examplesSophie

instance ToGoldenDirectory SophieNodeToNodeVersion
  -- Use defaults

instance ToGoldenDirectory (QueryVersion, SophieNodeToClientVersion) where
  toGoldenDirectory (queryVersion, blockVersion)
    = show queryVersion </> show blockVersion
