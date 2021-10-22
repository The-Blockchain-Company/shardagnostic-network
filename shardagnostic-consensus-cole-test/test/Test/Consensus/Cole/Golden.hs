{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Cole.Golden (tests) where

import           System.FilePath ((</>))

import           Shardagnostic.Consensus.Ledger.Query (QueryVersion)

import           Shardagnostic.Consensus.Cole.Ledger.NetworkProtocolVersion
import           Shardagnostic.Consensus.Cole.Node ()

import           Test.Tasty

import           Test.Util.Paths
import           Test.Util.Serialisation.Golden

import           Test.Consensus.Cole.Examples

tests :: TestTree
tests = goldenTest_all codecConfig $(getGoldenDir) examples

instance ToGoldenDirectory ColeNodeToNodeVersion
  -- Use defaults

instance ToGoldenDirectory (QueryVersion, ColeNodeToClientVersion) where
  toGoldenDirectory (queryVersion, blockVersion)
    = show queryVersion </> show blockVersion
