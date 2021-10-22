{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Shardagnostic.Consensus.Sophie.Ledger.Config (
    BlockConfig (..)
  , CodecConfig (..)
  , StorageConfig (..)
  , compactGenesis
  , getCompactGenesis
  , mkSophieBlockConfig
    -- * opaque
  , CompactGenesis
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))

import           Bcc.Binary (FromCBOR, ToCBOR)

import           Shardagnostic.Network.Magic (NetworkMagic (..))

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.BlockchainTime
import           Shardagnostic.Consensus.Config

import qualified Sophie.Spec.Ledger.API as SL

import           Shardagnostic.Consensus.Sophie.Eras (EraCrypto)
import           Shardagnostic.Consensus.Sophie.Ledger.Block

{-------------------------------------------------------------------------------
  Additional node configuration
-------------------------------------------------------------------------------}

data instance BlockConfig (SophieBlock era) = SophieConfig {
      -- | The highest protocol version this node supports. It will be stored
      -- the headers of produced blocks.
      sophieProtocolVersion  :: !SL.ProtVer
    , sophieSystemStart      :: !SystemStart
    , sophieNetworkMagic     :: !NetworkMagic
      -- | When chain selection is comparing two fragments, it will prefer the
      -- fragment with a tip signed by (one of) its own key(s) (provided that
      -- the 'BlockNo's and 'SlotNo's of the two tips are equal). For nodes that
      -- can produce blocks, this should be set to the verification key(s)
      -- corresponding to the node's signing key(s), to make sure we prefer
      -- self-issued blocks. For non block producing nodes, this can be set to
      -- the empty map.
    , sophieBlockIssuerVKeys :: !(Map (SL.KeyHash 'SL.BlockIssuer (EraCrypto era))
                                       (SL.VKey 'SL.BlockIssuer (EraCrypto era)))
    }
  deriving stock (Generic)

deriving instance SophieBasedEra era => Show     (BlockConfig (SophieBlock era))
deriving instance SophieBasedEra era => NoThunks (BlockConfig (SophieBlock era))

mkSophieBlockConfig ::
     SophieBasedEra era
  => SL.ProtVer
  -> SL.SophieGenesis era
  -> [SL.VKey 'SL.BlockIssuer (EraCrypto era)]
  -> BlockConfig (SophieBlock era)
mkSophieBlockConfig protVer genesis blockIssuerVKeys = SophieConfig {
      sophieProtocolVersion  = protVer
    , sophieSystemStart      = SystemStart  $ SL.sgSystemStart  genesis
    , sophieNetworkMagic     = NetworkMagic $ SL.sgNetworkMagic genesis
    , sophieBlockIssuerVKeys = Map.fromList
        [ (SL.hashKey k, k)
        | k <- blockIssuerVKeys
        ]
    }

{-------------------------------------------------------------------------------
  Codec config
-------------------------------------------------------------------------------}

-- | No particular codec configuration is needed for Sophie
data instance CodecConfig (SophieBlock era) = SophieCodecConfig
  deriving (Generic, NoThunks)

{-------------------------------------------------------------------------------
  Storage config
-------------------------------------------------------------------------------}

data instance StorageConfig (SophieBlock era) = SophieStorageConfig {
      -- | Needed for 'nodeCheckIntegrity'
      sophieStorageConfigSlotsPerKESPeriod :: !Word64
      -- | Needed for 'nodeImmutableDbChunkInfo'
    , sophieStorageConfigSecurityParam     :: !SecurityParam
    }
  deriving (Generic, NoThunks)

{-------------------------------------------------------------------------------
  Compact genesis
-------------------------------------------------------------------------------}

-- | Compact variant of 'SL.SophieGenesis' with some fields erased that are
-- only used on start-up and that should not be kept in memory forever.
--
-- Concretely:
--
-- * The 'sgInitialFunds' field is erased. It is only used to set up the initial
--   UTxO in tests and testnets.
--
-- * The 'sgStaking' field is erased. It is only used to register initial stake
--   pools in tests and benchmarks.
newtype CompactGenesis era = CompactGenesis {
      getCompactGenesis :: SL.SophieGenesis era
    }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromCBOR, ToCBOR)

deriving anyclass instance SophieBasedEra era => NoThunks (CompactGenesis era)

-- | Compacts the given 'SL.SophieGenesis'.
compactGenesis :: SL.SophieGenesis era -> CompactGenesis era
compactGenesis genesis = CompactGenesis $
    genesis {
        SL.sgInitialFunds = mempty
      , SL.sgStaking      = SL.emptyGenesisStaking
      }
