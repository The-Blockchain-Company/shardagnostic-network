{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Shardagnostic.Consensus.Cole.Ledger.Block (
    -- * Hash
    ColeHash (..)
  , mkColeHash
    -- * Block
  , ColeBlock (..)
  , annotateColeBlock
  , mkColeBlock
    -- * Header
  , Header (..)
  , mkBoundaryColeHeader
  , mkColeHeader
  , mkRegularColeHeader
    -- * Dealing with EBBs
  , coleBlockIsEBB
  , coleHeaderIsEBB
  , knownEBBs
    -- * Low-level API
  , UnsizedHeader (..)
  , joinSizeHint
  , mkUnsizedHeader
  , splitSizeHint
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as Strict
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))

import           Bcc.Binary

import qualified Crypto.Hash as Crypto

import qualified Bcc.Chain.Block as CC
import qualified Bcc.Chain.Cole.API as CC
import qualified Bcc.Chain.Slotting as CC
import qualified Bcc.Crypto.Hashing as CC

import           Shardagnostic.Network.DeltaQ (SizeInBytes)

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Util (ShowProxy (..))
import           Shardagnostic.Consensus.Util.Condense

import qualified Shardagnostic.Consensus.Cole.EBBs as EBBs
import           Shardagnostic.Consensus.Cole.Ledger.Conversions
import           Shardagnostic.Consensus.Cole.Ledger.Orphans ()

{-------------------------------------------------------------------------------
  Header hash
-------------------------------------------------------------------------------}

newtype ColeHash = ColeHash { unColeHash :: CC.HeaderHash }
  deriving stock   (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR, FromCBOR, Condense)
  deriving anyclass (NoThunks)

mkColeHash :: CC.ABlockOrBoundaryHdr ByteString -> ColeHash
mkColeHash = ColeHash . CC.abobHdrHash

instance ConvertRawHash ColeBlock where
  toShortRawHash   _ = CC.abstractHashToShort . unColeHash
  fromShortRawHash _ = ColeHash . CC.unsafeAbstractHashFromShort
  hashSize         _ = fromIntegral $ Crypto.hashDigestSize
                                        (error "proxy" :: Crypto.Blake2b_256)

{-------------------------------------------------------------------------------
  Block
-------------------------------------------------------------------------------}

-- | Cole block
--
-- We cache two bits of information:
--
-- * We cache the slot number as this is not readily available for EBBs.
--   Having it cached allows us to e.g. give a 'HasHeader' instance.
-- * We cache the hash as this is expensive to compute and we need it often.
data ColeBlock = ColeBlock {
      coleBlockRaw    :: !(CC.ABlockOrBoundary ByteString)
    , coleBlockSlotNo :: !SlotNo
    , coleBlockHash   :: !ColeHash
    }
  deriving (Eq, Show)

instance Condense ColeBlock where
  condense = condense . coleBlockRaw

instance ShowProxy ColeBlock where

mkColeBlock :: CC.EpochSlots -> CC.ABlockOrBoundary ByteString -> ColeBlock
mkColeBlock epochSlots blk = ColeBlock {
      coleBlockRaw    = blk
    , coleBlockSlotNo = fromColeSlotNo $ CC.abobHdrSlotNo epochSlots hdr
    , coleBlockHash   = mkColeHash hdr
    }
  where
    hdr = CC.abobHdrFromBlock blk

-- | Construct Cole block from unannotated 'CC.Block'
--
-- This should be used only when forging blocks (not when receiving blocks
-- over the wire).
annotateColeBlock :: CC.EpochSlots -> CC.Block -> ColeBlock
annotateColeBlock es = mkColeBlock es . CC.ABOBBlock . CC.reAnnotateBlock es

{-------------------------------------------------------------------------------
  Header
-------------------------------------------------------------------------------}

-- | Cole header
--
-- See 'ColeBlock' for comments on why we cache certain values.
data instance Header ColeBlock = ColeHeader {
      coleHeaderRaw    :: !(CC.ABlockOrBoundaryHdr ByteString)
    , coleHeaderSlotNo :: !SlotNo
    , coleHeaderHash   :: !ColeHash

      -- | Hint about the block size
      --
      -- This is used only for the block fetch client. When this value is
      -- wrong, block fetch might make suboptimal decisions, but it shouldn't
      -- /break/ anything
    , coleHeaderBlockSizeHint :: !SizeInBytes
    }
  deriving (Eq, Show, Generic)

instance GetHeader ColeBlock where
  getHeader ColeBlock{..} = ColeHeader {
        coleHeaderRaw           = CC.abobHdrFromBlock coleBlockRaw
      , coleHeaderSlotNo        = coleBlockSlotNo
      , coleHeaderHash          = coleBlockHash
      , coleHeaderBlockSizeHint = (+ overhead) . fromIntegral . Strict.length $
          -- For some reason regular blocks lack a 'Decoded' instance
          case coleBlockRaw of
            CC.ABOBBlock    blk -> CC.blockAnnotation blk
            CC.ABOBBoundary blk -> recoverBytes       blk
      }
    where
      -- The maximum block size is 65536, the CBOR-in-CBOR tag for this block
      -- is:
      --
      -- > D8 18          # tag(24)
      -- >    1A 00010000 # bytes(65536)
      --
      -- Which is 7 bytes, enough for up to 4294967295 bytes.
      overhead = 7 {- CBOR-in-CBOR -} + 2 {- EBB tag -}

  -- Check if a block matches its header
  --
  -- Note that we cannot check this for an EBB, as the EBB header doesn't
  -- store a hash of the EBB body.
  blockMatchesHeader hdr blk =
      CC.abobMatchesBody (coleHeaderRaw hdr) (coleBlockRaw blk)

  headerIsEBB hdr = case coleHeaderRaw hdr of
    CC.ABOBBlockHdr _       -> Nothing
    CC.ABOBBoundaryHdr bhdr -> Just
                              . EpochNo
                              . CC.boundaryEpoch
                              $ bhdr

instance Condense (Header ColeBlock) where
  condense = CC.aBlockOrBoundaryHdr condense condense . coleHeaderRaw

instance ShowProxy (Header ColeBlock) where

instance NoThunks (Header ColeBlock) where
  showTypeOf _ = show $ typeRep (Proxy @(Header ColeBlock))

mkColeHeader :: CC.EpochSlots
              -> CC.ABlockOrBoundaryHdr ByteString
              -> SizeInBytes -- ^ Block size hint
              -> Header ColeBlock
mkColeHeader epochSlots = joinSizeHint . mkUnsizedHeader epochSlots

mkRegularColeHeader :: CC.AHeader ByteString
                     -> SizeInBytes
                     -> Header ColeBlock
mkRegularColeHeader = joinSizeHint . mkRegularUnsizedHeader

mkBoundaryColeHeader :: SlotNo
                      -> CC.ABoundaryHeader ByteString
                      -> SizeInBytes
                      -> Header ColeBlock
mkBoundaryColeHeader slotNo = joinSizeHint . mkBoundaryUnsizedHeader slotNo

{-------------------------------------------------------------------------------
  HasHeader instances

  This doesn't do much more than pass to the instance for headers.
-------------------------------------------------------------------------------}

type instance HeaderHash ColeBlock = ColeHash
instance StandardHash ColeBlock

instance HasHeader ColeBlock where
  getHeaderFields = getBlockHeaderFields

instance HasHeader (Header ColeBlock) where
  getHeaderFields hdr = HeaderFields {
        headerFieldHash    = coleHeaderHash hdr
      , headerFieldSlot    = coleHeaderSlotNo hdr
      , headerFieldBlockNo = fromColeBlockNo . CC.abobHdrChainDifficulty $ coleHeaderRaw hdr
      }

instance GetPrevHash ColeBlock where
  headerPrevHash = fromColePrevHash . CC.abobHdrPrevHash . coleHeaderRaw

fromColePrevHash :: Maybe CC.HeaderHash -> ChainHash ColeBlock
fromColePrevHash = \case
    Nothing -> GenesisHash
    Just h  -> BlockHash (ColeHash h)

{-------------------------------------------------------------------------------
  Dealing with EBBs
-------------------------------------------------------------------------------}

coleHeaderIsEBB :: Header ColeBlock -> IsEBB
coleHeaderIsEBB = go . coleHeaderRaw
  where
    go :: CC.ABlockOrBoundaryHdr a -> IsEBB
    go (CC.ABOBBlockHdr    _) = IsNotEBB
    go (CC.ABOBBoundaryHdr _) = IsEBB

coleBlockIsEBB :: ColeBlock -> IsEBB
coleBlockIsEBB = coleHeaderIsEBB . getHeader

knownEBBs :: Map (HeaderHash ColeBlock) (ChainHash ColeBlock)
knownEBBs = Map.fromList $ map aux EBBs.knownEBBs
  where
    aux :: (CC.HeaderHash, Maybe CC.HeaderHash)
        -> (ColeHash, ChainHash ColeBlock)
    aux (ebb, Nothing)   = (ColeHash ebb, GenesisHash)
    aux (ebb, Just prev) = (ColeHash ebb, BlockHash (ColeHash prev))

{-------------------------------------------------------------------------------
  Unsized header
-------------------------------------------------------------------------------}

-- | Header without a size hint
--
-- Defined in order to support backwards compatible binary encodings.
data UnsizedHeader = UnsizedHeader {
      unsizedHeaderRaw    :: !(CC.ABlockOrBoundaryHdr ByteString)
    , unsizedHeaderSlotNo :: !SlotNo
    , unsizedHeaderHash   :: !ColeHash
    }

mkUnsizedHeader :: CC.EpochSlots
                -> CC.ABlockOrBoundaryHdr ByteString
                -> UnsizedHeader
mkUnsizedHeader epochSlots = \case
    CC.ABOBBlockHdr    hdr -> mkRegularUnsizedHeader hdr
    CC.ABOBBoundaryHdr hdr -> mkBoundaryUnsizedHeader slotNo hdr
      where
        slotNo = fromColeSlotNo $
            CC.boundaryBlockSlot epochSlots (CC.boundaryEpoch hdr)

mkRegularUnsizedHeader :: CC.AHeader ByteString -> UnsizedHeader
mkRegularUnsizedHeader hdr = UnsizedHeader {
      unsizedHeaderRaw    = hdr'
    , unsizedHeaderSlotNo = fromColeSlotNo $ CC.headerSlot hdr
    , unsizedHeaderHash   = mkColeHash hdr'
    }
  where
    hdr' :: CC.ABlockOrBoundaryHdr ByteString
    hdr' = CC.ABOBBlockHdr hdr

-- | For a boundary header, we must be told the slot
mkBoundaryUnsizedHeader :: SlotNo
                        -> CC.ABoundaryHeader ByteString
                        -> UnsizedHeader
mkBoundaryUnsizedHeader slotNo hdr = UnsizedHeader {
      unsizedHeaderRaw    = hdr'
    , unsizedHeaderSlotNo = slotNo
    , unsizedHeaderHash   = mkColeHash hdr'
    }
  where
    hdr' :: CC.ABlockOrBoundaryHdr ByteString
    hdr' = CC.ABOBBoundaryHdr hdr

splitSizeHint :: Header ColeBlock -> (UnsizedHeader, SizeInBytes)
splitSizeHint ColeHeader{..} = (
      UnsizedHeader {
          unsizedHeaderRaw    = coleHeaderRaw
        , unsizedHeaderSlotNo = coleHeaderSlotNo
        , unsizedHeaderHash   = coleHeaderHash
        }
    , coleHeaderBlockSizeHint
    )

joinSizeHint :: UnsizedHeader -> SizeInBytes -> Header ColeBlock
joinSizeHint UnsizedHeader{..} size = ColeHeader {
      coleHeaderRaw           = unsizedHeaderRaw
    , coleHeaderSlotNo        = unsizedHeaderSlotNo
    , coleHeaderHash          = unsizedHeaderHash
    , coleHeaderBlockSizeHint = size
    }
