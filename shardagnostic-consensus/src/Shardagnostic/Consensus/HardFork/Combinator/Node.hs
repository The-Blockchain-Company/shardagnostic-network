{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Shardagnostic.Consensus.HardFork.Combinator.Node () where

import           Data.Proxy
import           Data.SOP.Strict
import           GHC.Stack

import           Shardagnostic.Consensus.Config.SupportsNode
import           Shardagnostic.Consensus.Node.NetworkProtocolVersion
import           Shardagnostic.Consensus.Node.Run

import           Shardagnostic.Consensus.HardFork.Combinator.Abstract
import           Shardagnostic.Consensus.HardFork.Combinator.AcrossEras
import           Shardagnostic.Consensus.HardFork.Combinator.Basics
import           Shardagnostic.Consensus.HardFork.Combinator.Forging ()
import           Shardagnostic.Consensus.HardFork.Combinator.Ledger.CommonProtocolParams ()
import           Shardagnostic.Consensus.HardFork.Combinator.Ledger.PeerSelection ()
import           Shardagnostic.Consensus.HardFork.Combinator.Node.InitStorage ()
import           Shardagnostic.Consensus.HardFork.Combinator.Node.Metrics ()
import           Shardagnostic.Consensus.HardFork.Combinator.Serialisation

{-------------------------------------------------------------------------------
  ConfigSupportsNode
-------------------------------------------------------------------------------}

instance CanHardFork xs => ConfigSupportsNode (HardForkBlock xs) where
  getSystemStart  = getSameConfigValue getSystemStart
  getNetworkMagic = getSameConfigValue getNetworkMagic

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getSameConfigValue
  :: forall xs a. (CanHardFork xs, Eq a, HasCallStack)
  => (forall blk. ConfigSupportsNode blk => BlockConfig blk -> a)
  -> BlockConfig (HardForkBlock xs)
  -> a
getSameConfigValue getValue blockConfig = getSameValue values
  where
    values :: NP (K a) xs
    values =
          hcmap (Proxy @SingleEraBlock) (K . getValue)
        . getPerEraBlockConfig
        . hardForkBlockConfigPerEra
        $ blockConfig

{-------------------------------------------------------------------------------
  RunNode
-------------------------------------------------------------------------------}

instance ( CanHardFork xs
           -- Instances that must be defined for specific values of @b@:
         , SupportedNetworkProtocolVersion (HardForkBlock xs)
         , SerialiseHFC xs
         ) => RunNode (HardForkBlock xs)
