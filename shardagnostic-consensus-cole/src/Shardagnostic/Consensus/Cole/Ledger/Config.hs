{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Shardagnostic.Consensus.Cole.Ledger.Config (
    -- * Block config
    BlockConfig (..)
  , coleEpochSlots
  , coleGenesisHash
  , coleProtocolMagic
  , coleProtocolMagicId
    -- * Codec config
  , CodecConfig (..)
  , mkColeCodecConfig
    -- * Storage config
  , StorageConfig (..)
    -- * Compact genesis config
  , compactGenesisConfig
  ) where

import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))

import qualified Bcc.Chain.Genesis as CC.Genesis
import qualified Bcc.Chain.Slotting as CC.Slot
import qualified Bcc.Chain.Update as CC.Update
import qualified Bcc.Crypto as Crypto

import           Shardagnostic.Consensus.Block

import           Shardagnostic.Consensus.Cole.Ledger.Block

{-------------------------------------------------------------------------------
  Block config
-------------------------------------------------------------------------------}

-- | Extended configuration we need for Cole
data instance BlockConfig ColeBlock = ColeConfig {
      -- | Genesis configuration
      coleGenesisConfig   :: !CC.Genesis.Config

      -- | Node protocol version
      --
      -- NOTE: This is /static/ for the node, and may not correspond to what's
      -- on the chain. It's the protocol supported by /this/ node; to change it,
      -- you'd have to change the software.
    , coleProtocolVersion :: !CC.Update.ProtocolVersion

      -- | Node software version
      --
      -- Like 'coleProtocolVersion', this is independent from the chain.
    , coleSoftwareVersion :: !CC.Update.SoftwareVersion
    }
  deriving (Generic, NoThunks)

coleGenesisHash :: BlockConfig ColeBlock -> CC.Genesis.GenesisHash
coleGenesisHash = CC.Genesis.configGenesisHash . coleGenesisConfig

coleProtocolMagicId :: BlockConfig ColeBlock -> Crypto.ProtocolMagicId
coleProtocolMagicId = Crypto.getProtocolMagicId . coleProtocolMagic

coleProtocolMagic :: BlockConfig ColeBlock -> Crypto.ProtocolMagic
coleProtocolMagic = CC.Genesis.configProtocolMagic . coleGenesisConfig

coleEpochSlots :: BlockConfig ColeBlock -> CC.Slot.EpochSlots
coleEpochSlots = CC.Genesis.configEpochSlots . coleGenesisConfig

{-------------------------------------------------------------------------------
  Codec config
-------------------------------------------------------------------------------}

newtype instance CodecConfig ColeBlock = ColeCodecConfig {
      getColeEpochSlots :: CC.Slot.EpochSlots
    }
  deriving (Generic, NoThunks)

mkColeCodecConfig :: CC.Genesis.Config -> CodecConfig ColeBlock
mkColeCodecConfig cfg = ColeCodecConfig {
      getColeEpochSlots = CC.Genesis.configEpochSlots cfg
    }

{-------------------------------------------------------------------------------
  Storage config
-------------------------------------------------------------------------------}

newtype instance StorageConfig ColeBlock = ColeStorageConfig {
      -- | We need the 'BlockConfig' to be able to forge an EBB in
      -- 'nodeInitChainDB'.
      getColeBlockConfig :: BlockConfig ColeBlock
    }
  deriving (Generic, NoThunks)

{-------------------------------------------------------------------------------
  Compact genesis config
-------------------------------------------------------------------------------}

-- | Cole's genesis config contains the AVVM balances, of which there are +14k
-- in mainnet's genesis config. These balances are only used to create the
-- initial ledger state, there is no reason to keep them in memory afterwards.
--
-- This function empties the 'gdAvvmDistr' field in the genesis config. As we
-- keep Cole's genesis config in memory (even in later eras), this can save us
-- a bit of memory.
compactGenesisConfig :: CC.Genesis.Config -> CC.Genesis.Config
compactGenesisConfig cfg = cfg {
      CC.Genesis.configGenesisData = (CC.Genesis.configGenesisData cfg) {
          CC.Genesis.gdAvvmDistr = CC.Genesis.GenesisAvvmBalances Map.empty
        }
    }
