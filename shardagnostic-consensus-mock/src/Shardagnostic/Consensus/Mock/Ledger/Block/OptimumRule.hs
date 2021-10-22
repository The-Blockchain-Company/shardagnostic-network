{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test the Optimum chain selection rule (with explicit leader schedule)
module Shardagnostic.Consensus.Mock.Ledger.Block.OptimumRule (
    OptimumCryptoUnused
  , SimpleOptimumRuleBlock
  , SimpleOptimumRuleExt (..)
  , SimpleOptimumRuleHeader
  , forgeOptimumRuleExt
  ) where

import           Codec.Serialise (Serialise (..))
import           Data.Void (Void)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Bcc.Crypto.Hash
import           Bcc.Crypto.KES
import           Bcc.Crypto.VRF

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Config
import           Shardagnostic.Consensus.Forecast
import           Shardagnostic.Consensus.Ledger.SupportsProtocol
import           Shardagnostic.Consensus.Mock.Ledger.Block
import           Shardagnostic.Consensus.Mock.Ledger.Forge
import           Shardagnostic.Consensus.Mock.Node.Abstract
import           Shardagnostic.Consensus.Mock.Protocol.LeaderSchedule
import           Shardagnostic.Consensus.Mock.Protocol.Optimum
import           Shardagnostic.Consensus.NodeId (CoreNodeId)
import           Shardagnostic.Consensus.Util.Condense

import           Shardagnostic.Consensus.Storage.Serialisation

{-------------------------------------------------------------------------------
  Instantiate @ext@
-------------------------------------------------------------------------------}

-- | Simple block extended with the fields required for Optimum
--
-- @c@ is crypto used for the block itself
-- With an explicit leader schedule we need no crypto for the consensus protocol.
--
-- This is an example of a block which is /not/ an instance of 'SignedBlock'.
type SimpleOptimumRuleBlock c = SimpleBlock c SimpleOptimumRuleExt

-- | Header for Proas
type SimpleOptimumRuleHeader c = SimpleHeader c SimpleOptimumRuleExt

-- | Required extension
--
-- The 'WithLeaderSchedule' doesn't require /anything/ in the block header.
-- We add the 'CoreNodeId' just so that we can check that the schedule matches
-- the chain.
newtype SimpleOptimumRuleExt = SimpleOptimumRuleExt {
      simpleOptimumRuleExt :: CoreNodeId
    }
  deriving stock    (Generic, Show, Eq)
  deriving newtype  (Condense)
  deriving anyclass (NoThunks)

type instance BlockProtocol (SimpleOptimumRuleBlock c) =
    WithLeaderSchedule (Optimum OptimumCryptoUnused)

-- | Sanity check that block and header type synonyms agree
_simpleOptimumRuleHeader :: SimpleOptimumRuleBlock c -> SimpleOptimumRuleHeader c
_simpleOptimumRuleHeader = simpleHeader

{-------------------------------------------------------------------------------
  Customization of the generic infrastructure
-------------------------------------------------------------------------------}

instance SimpleCrypto c => MockProtocolSpecific c SimpleOptimumRuleExt where
  type MockLedgerConfig c SimpleOptimumRuleExt = ()

{-------------------------------------------------------------------------------
  Evidence that 'SimpleBlock' can support Optimum with an explicit leader schedule
-------------------------------------------------------------------------------}

instance SimpleCrypto c => RunMockBlock c SimpleOptimumRuleExt where
  mockNetworkMagic = const constructMockNetworkMagic

instance
  ( SimpleCrypto c
  ) => BlockSupportsProtocol (SimpleBlock c SimpleOptimumRuleExt) where
  validateView _ _ = ()

instance
  ( SimpleCrypto c
  ) => LedgerSupportsProtocol (SimpleOptimumRuleBlock c) where
  protocolLedgerView   _ _ = TickedTrivial
  ledgerViewForecastAt _   = trivialForecast

{-------------------------------------------------------------------------------
  We don't need crypto for this protocol
-------------------------------------------------------------------------------}

data OptimumCryptoUnused

instance OptimumCrypto OptimumCryptoUnused where
  type OptimumKES  OptimumCryptoUnused = NeverKES
  type OptimumVRF  OptimumCryptoUnused = NeverVRF
  type OptimumHash OptimumCryptoUnused = NeverHash

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}


type instance CannotForge (SimpleOptimumRuleBlock c) = Void

type instance ForgeStateInfo (SimpleOptimumRuleBlock c) = ()

type instance ForgeStateUpdateError (SimpleOptimumRuleBlock c) = Void

forgeOptimumRuleExt :: SimpleCrypto c => ForgeExt c SimpleOptimumRuleExt
forgeOptimumRuleExt = ForgeExt $ \cfg _ SimpleBlock{..} ->
    let ext = SimpleOptimumRuleExt $ wlsConfigNodeId (configConsensus cfg)
        SimpleHeader{..} = simpleHeader
    in SimpleBlock {
        simpleHeader = mkSimpleHeader encode simpleHeaderStd ext
      , simpleBody   = simpleBody
      }

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance Serialise SimpleOptimumRuleExt

instance EncodeDisk (SimpleOptimumRuleBlock c) ()
  -- Default instance

instance DecodeDisk (SimpleOptimumRuleBlock c) ()
  -- Default instance
