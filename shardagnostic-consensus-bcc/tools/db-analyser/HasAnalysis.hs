{-# LANGUAGE TypeFamilies #-}
module HasAnalysis (
    HasAnalysis (..)
  , HasProtocolInfo (..)
  , SizeInBytes
  ) where

import           Data.Map.Strict (Map)
import           Options.Applicative

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Node.ProtocolInfo
import           Shardagnostic.Consensus.Storage.Serialisation (SizeInBytes)

{-------------------------------------------------------------------------------
  HasAnalysis
-------------------------------------------------------------------------------}

class GetPrevHash blk => HasAnalysis blk where
  countTxOutputs :: blk -> Int
  blockTxSizes   :: blk -> [SizeInBytes]
  knownEBBs      :: proxy blk -> Map (HeaderHash blk) (ChainHash blk)

class HasProtocolInfo blk where
  data Args blk
  argsParser     :: proxy blk -> Parser (Args blk)
  mkProtocolInfo :: Args blk -> IO (ProtocolInfo IO blk)
