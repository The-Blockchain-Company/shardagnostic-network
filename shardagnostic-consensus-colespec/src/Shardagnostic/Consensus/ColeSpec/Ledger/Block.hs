{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Shardagnostic.Consensus.ColeSpec.Ledger.Block (
    BlockConfig (..)
  , ColeSpecBlock (..)
  , CodecConfig (..)
  , Header (..)
  , StorageConfig (..)
    -- * type alias
  , ColeSpecHeader
  ) where

import           Codec.Serialise
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import qualified Cole.Spec.Chain.STS.Block as Spec
import qualified Cole.Spec.Ledger.Core as Spec

import           Shardagnostic.Consensus.Block

import           Shardagnostic.Consensus.ColeSpec.Ledger.Conversions
import           Shardagnostic.Consensus.ColeSpec.Ledger.Orphans ()

{-------------------------------------------------------------------------------
  Block
-------------------------------------------------------------------------------}

-- | Block according to the Cole spec
--
-- Just like we do for 'ColeBlock', we cache the header hash. In addition, we
-- also add the 'BlockNo', as this is entirely absent from the spec but we need
-- it for the 'HasHeader' abstraction, which is ubiquitous in
-- @shardagnostic-consensus@ and @-network@.
data ColeSpecBlock = ColeSpecBlock {
      coleSpecBlock     :: Spec.Block
    , coleSpecBlockNo   :: BlockNo
    , coleSpecBlockHash :: Spec.Hash
    }
  deriving (Show, Eq, Generic, Serialise)

{-------------------------------------------------------------------------------
  GetHeader
-------------------------------------------------------------------------------}

data instance Header ColeSpecBlock = ColeSpecHeader {
      coleSpecHeader     :: Spec.BlockHeader
    , coleSpecHeaderNo   :: BlockNo
    , coleSpecHeaderHash :: Spec.Hash
    }
  deriving (Show, Eq, Generic, Serialise)

instance GetHeader ColeSpecBlock where
  getHeader ColeSpecBlock{..} = ColeSpecHeader {
        coleSpecHeader     = Spec._bHeader coleSpecBlock
      , coleSpecHeaderNo   = coleSpecBlockNo
      , coleSpecHeaderHash = coleSpecBlockHash
      }

  -- We don't care about integrity checks, so we don't bother checking whether
  -- the hashes of the body are correct
  blockMatchesHeader hdr blk = blockHash hdr == blockHash blk

  -- No EBBs
  headerIsEBB = const Nothing

type ColeSpecHeader = Header ColeSpecBlock

{-------------------------------------------------------------------------------
  HasHeader
-------------------------------------------------------------------------------}

type instance HeaderHash ColeSpecBlock = Spec.Hash
instance StandardHash ColeSpecBlock

instance HasHeader ColeSpecBlock where
  getHeaderFields = getBlockHeaderFields

instance HasHeader ColeSpecHeader where
  getHeaderFields hdr = HeaderFields {
        headerFieldHash    = coleSpecHeaderHash hdr
      , headerFieldBlockNo = coleSpecHeaderNo hdr
      , headerFieldSlot    = fromColeSpecSlotNo . Spec._bhSlot $ coleSpecHeader hdr
      }

instance GetPrevHash ColeSpecBlock where
  headerPrevHash = fromColeSpecPrevHash id . Spec._bhPrevHash . coleSpecHeader

{-------------------------------------------------------------------------------
  Config
-------------------------------------------------------------------------------}

data instance BlockConfig ColeSpecBlock = ColeSpecBlockConfig
  deriving (Generic, NoThunks)

data instance CodecConfig ColeSpecBlock = ColeSpecCodecConfig
  deriving (Generic, NoThunks)

data instance StorageConfig ColeSpecBlock = ColeSpecStorageConfig
  deriving (Generic, NoThunks)
