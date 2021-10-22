module Shardagnostic.Consensus.Config.SupportsNode (ConfigSupportsNode (..)) where

import           Shardagnostic.Network.Magic (NetworkMagic)

import           Shardagnostic.Consensus.Block.Abstract (BlockConfig)
import           Shardagnostic.Consensus.BlockchainTime (SystemStart)

-- | The 'BlockConfig' needs to contain some information in order to support
-- running a node.
class ConfigSupportsNode blk where
  getSystemStart  :: BlockConfig blk -> SystemStart
  getNetworkMagic :: BlockConfig blk -> NetworkMagic
