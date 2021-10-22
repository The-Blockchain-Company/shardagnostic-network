{-# OPTIONS_GHC -Wno-orphans #-}
module Shardagnostic.Consensus.HardFork.Combinator.Node.Metrics () where

import           Data.SOP.Strict

import           Shardagnostic.Consensus.Block.SupportsMetrics
import           Shardagnostic.Consensus.Util

import           Shardagnostic.Consensus.HardFork.Combinator.Abstract
import           Shardagnostic.Consensus.HardFork.Combinator.AcrossEras
import           Shardagnostic.Consensus.HardFork.Combinator.Basics
import           Shardagnostic.Consensus.HardFork.Combinator.Block

instance CanHardFork xs => BlockSupportsMetrics (HardForkBlock xs) where
  isSelfIssued cfg hdr =
        hcollapse
      $ hczipWith
          proxySingle
          (K .: isSelfIssued)
          (getPerEraBlockConfig $ hardForkBlockConfigPerEra cfg)
          (getOneEraHeader      $ getHardForkHeader         hdr)
