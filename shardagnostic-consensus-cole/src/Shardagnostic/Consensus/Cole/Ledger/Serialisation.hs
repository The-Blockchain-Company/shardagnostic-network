{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Shardagnostic.Consensus.Cole.Ledger.Serialisation (
    -- * Data family instances
    NestedCtxt_ (..)
  , RawBoundaryHeader
  , RawHeader
    -- * Serialisation
  , coleBlockEncodingOverhead
  , decodeColeBlock
  , decodeColeBoundaryBlock
  , decodeColeBoundaryHeader
  , decodeColeHeaderHash
  , decodeColeRegularBlock
  , decodeColeRegularHeader
  , encodeColeBlock
  , encodeColeBoundaryHeader
  , encodeColeHeaderHash
  , encodeColeRegularHeader
    -- * Support for on-disk format
  , coleBinaryBlockInfo
    -- * Unsized header
  , addV1Envelope
  , decodeUnsizedHeader
  , dropV1Envelope
  , encodeUnsizedHeader
  , fakeColeBlockSizeHint
  ) where

import           Control.Monad.Except
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Word (Word32)

import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise (..))

import           Bcc.Binary

import qualified Bcc.Chain.Block as CC
import qualified Bcc.Chain.Slotting as CC

import           Shardagnostic.Network.DeltaQ (SizeInBytes)

import           Shardagnostic.Consensus.Block

import           Shardagnostic.Consensus.Storage.Common (BinaryBlockInfo (..))

import           Shardagnostic.Consensus.Cole.Ledger.Block
import           Shardagnostic.Consensus.Cole.Ledger.Orphans ()

{-------------------------------------------------------------------------------
  Serialise instances

  Mostly we don't depend on Serialise, but use explicit functions instead.
-------------------------------------------------------------------------------}

instance Serialise ColeHash where
  decode = decodeColeHeaderHash
  encode = encodeColeHeaderHash

{-------------------------------------------------------------------------------
  Type synonyms
-------------------------------------------------------------------------------}

type RawBoundaryHeader = CC.ABoundaryHeader Strict.ByteString
type RawHeader         = CC.AHeader         Strict.ByteString

{-------------------------------------------------------------------------------
  Nested contents
-------------------------------------------------------------------------------}

-- | Since the Cole header does not contain the size, we include it in the
-- nested type instead.
data instance NestedCtxt_ ColeBlock f a where
  CtxtColeRegular ::
       !SizeInBytes
    -> NestedCtxt_ ColeBlock Header RawHeader

  -- | In order to reconstruct 'Header ColeBlock' we need the 'SlotNo'
  --
  -- We could compute that using 'EpochSlots', but we don't have that available
  -- here.
  CtxtColeBoundary ::
       !SizeInBytes
    -> NestedCtxt_ ColeBlock Header (SlotNo, RawBoundaryHeader)

deriving instance Show (NestedCtxt_ ColeBlock f a)

instance SameDepIndex (NestedCtxt_ ColeBlock f) where
  sameDepIndex (CtxtColeRegular size) (CtxtColeRegular size') = do
      guard (size == size')
      return Refl
  sameDepIndex (CtxtColeBoundary size) (CtxtColeBoundary size') = do
      guard (size == size')
      return Refl
  sameDepIndex _ _ =
      Nothing

instance HasNestedContent Header ColeBlock where
  unnest hdr = case coleHeaderRaw hdr of
      CC.ABOBBoundaryHdr h -> DepPair (NestedCtxt (CtxtColeBoundary blockSize)) (slotNo, h)
      CC.ABOBBlockHdr    h -> DepPair (NestedCtxt (CtxtColeRegular  blockSize)) h
    where
      blockSize = coleHeaderBlockSizeHint hdr
      slotNo    = coleHeaderSlotNo        hdr

  nest = \case
      DepPair (NestedCtxt (CtxtColeBoundary blockSize)) (slotNo, h) ->
        mkBoundaryColeHeader slotNo h blockSize
      DepPair (NestedCtxt (CtxtColeRegular blockSize)) h ->
        mkRegularColeHeader h blockSize

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

-- | The Cole block encoding overhead size in bytes.
--
-- This encompasses the overhead in bytes for everything that is encoded
-- within a Cole block, excluding the actual generalized transactions
-- (transactions, delegation certificates, update votes, and update
-- proposals).
coleBlockEncodingOverhead :: Word32
coleBlockEncodingOverhead =
    blockHeaderOverhead + blockBodyOverhead + safetyMargin
  where
    -- The maximum block header size.
    blockHeaderOverhead = 650

    -- The block body overhead excluding the actual generalized transactions.
    blockBodyOverhead = 1 {- ABody: encodeListLen 4 -}
                      + 2 {- TxPayload: list -}
                      + 1 {- SscPayload: encodeListLen 2 -}
                      + 1 {- SscPayload: Word8 -}
                      + 1 {- SscPayload: mempty :: Set () -}
                      + 2 {- Delegation.Payload: list -}
                      + 1 {- Update.Payload: encodeListLen 2 -}
                      + 1 {- Update.Payload: Maybe AProposal -}
                      + 2 {- Update.Payload: list of AVote -}

    -- Just for safety.
    safetyMargin = 1024

encodeColeHeaderHash :: HeaderHash ColeBlock -> Encoding
encodeColeHeaderHash = toCBOR

decodeColeHeaderHash :: Decoder s (HeaderHash ColeBlock)
decodeColeHeaderHash = fromCBOR

-- | Encode a block
--
-- Should be backwards compatible with legacy (bcc-sl) nodes.
--
-- Implementation note: the decoder uses 'CC.fromCBORABlockOrBoundary', which
-- has inverse 'CC.toCBORABlockOrBoundary'. This encoder is intended to be
-- binary compatible with 'CC.toCBORABlockOrBoundary', but does not use it and
-- instead takes advantage of the annotations (using 'encodePreEncoded').
encodeColeBlock :: ColeBlock -> Encoding
encodeColeBlock blk = mconcat [
      CBOR.encodeListLen 2
    , case coleBlockRaw blk of
        CC.ABOBBoundary b -> mconcat [
            CBOR.encodeWord 0
          , CBOR.encodePreEncoded $ CC.boundaryAnnotation b
          ]
        CC.ABOBBlock b -> mconcat [
            CBOR.encodeWord 1
          , CBOR.encodePreEncoded $ CC.blockAnnotation b
          ]
    ]

-- | Inverse of 'encodeColeBlock'
decodeColeBlock :: CC.EpochSlots -> Decoder s (Lazy.ByteString -> ColeBlock)
decodeColeBlock epochSlots =
    flip (\bs -> mkColeBlock epochSlots
               . annotationBytes bs)
    <$> CC.fromCBORABlockOrBoundary epochSlots

-- | Decoder for a regular (non-EBB) Cole block.
--
-- PRECONDITION: the 'Lazy.ByteString' given as argument to the decoder is the
-- same as the one that is decoded.
--
-- This is a wrapper for 'CC.fromCBORABlock'.
--
-- Use 'decodeColeBlock' when you can, this function is provided for use by
-- the hard-fork combinator.
decodeColeRegularBlock :: CC.EpochSlots
                        -> Decoder s (Lazy.ByteString -> ColeBlock)
decodeColeRegularBlock epochSlots =
    flip (\bs -> mkColeBlock epochSlots
               . annotationBytes bs
               . CC.ABOBBlock)
    <$> CC.fromCBORABlock epochSlots

-- | Decoder for a boundary Cole block.
--
-- PRECONDITION: the 'Lazy.ByteString' given as argument to the decoder is the
-- same as the one that is decoded.
--
-- This is a wrapper for 'CC.fromCBORABoundaryBlock'.
--
-- Use 'decodeColeBlock' when you can, this function is provided for use by
-- the hard-fork combinator.
decodeColeBoundaryBlock :: CC.EpochSlots
                         -> Decoder s (Lazy.ByteString -> ColeBlock)
decodeColeBoundaryBlock epochSlots =
    flip (\bs -> mkColeBlock epochSlots
               . annotationBytes bs
               . CC.ABOBBoundary)
    <$> CC.fromCBORABoundaryBlock

-- | Encodes a raw Cole header /without/ a tag indicating whether it's a
-- regular header or an EBB header.
--
-- Uses the annotation, so cheap.
encodeColeRegularHeader :: RawHeader -> Encoding
encodeColeRegularHeader = CBOR.encodePreEncoded . CC.headerAnnotation

-- | Inverse of 'encodeColeRegularHeader'
decodeColeRegularHeader
  :: CC.EpochSlots
  -> Decoder s (Lazy.ByteString -> RawHeader)
decodeColeRegularHeader epochSlots =
    flip annotationBytes <$> CC.fromCBORAHeader epochSlots

-- | Encodes a raw Cole EBB header /without/ a tag indicating whether it's a
-- regular header or an EBB header.
--
-- Uses the annotation, so cheap.
encodeColeBoundaryHeader :: RawBoundaryHeader -> Encoding
encodeColeBoundaryHeader = CBOR.encodePreEncoded . CC.boundaryHeaderAnnotation

-- | Inverse of 'encodeColeBoundaryHeader'
decodeColeBoundaryHeader :: Decoder s (Lazy.ByteString -> RawBoundaryHeader)
decodeColeBoundaryHeader =
    flip annotationBytes <$> CC.fromCBORABoundaryHeader

-- | The 'BinaryBlockInfo' of the given 'ColeBlock'.
--
-- NOTE: the bytestring obtained by slicing the serialised block using the
-- header offset and size will correspond to the /header annotation/, but not
-- to the serialised header, as we add an envelope ('encodeListLen' + tag)
-- around a header in 'encodeColeHeader'. This envelope must thus still be
-- added to the sliced bytestring before it can be deserialised using
-- 'decodeColeHeader'.
coleBinaryBlockInfo :: ColeBlock -> BinaryBlockInfo
coleBinaryBlockInfo blk = BinaryBlockInfo
    { headerOffset = 1 {- 'encodeListLen' of the outer 'Either' envelope -}
                   + 1 {- the tag -}
                   + 1 {- 'encodeListLen' of the block: header + body + ...  -}
      -- Compute the length of the annotated header
    , headerSize   = fromIntegral $ Strict.length $ case coleBlockRaw blk of
        CC.ABOBBoundary b -> CC.boundaryHeaderAnnotation $ CC.boundaryHeader b
        CC.ABOBBlock    b -> CC.headerAnnotation         $ CC.blockHeader    b
    }

{-------------------------------------------------------------------------------
  V1 envelope: unsized header

  These are auxiliary functions for encoding/decoding the Cole header.
-------------------------------------------------------------------------------}

-- | A 'CC.ABlockOrBoundary' is a CBOR 2-tuple of a 'Word' (0 = EBB, 1 =
-- regular block) and block/ebb payload. This function returns the bytes that
-- should be prepended to the payload, i.e., the byte indicating it's a CBOR
-- 2-tuple and the 'Word' indicating whether its an EBB or regular block.
isEbbEnvelope :: IsEBB -> Lazy.ByteString
isEbbEnvelope = \case
  IsEBB    -> "\130\NUL"
  IsNotEBB -> "\130\SOH"

addV1Envelope ::
     (SomeSecond (NestedCtxt Header) ColeBlock, Lazy.ByteString)
  -> Lazy.ByteString
addV1Envelope (SomeSecond (NestedCtxt ctxt), bs) = isEbbTag <> bs
  where
    isEbbTag = case ctxt of
      CtxtColeBoundary {} -> isEbbEnvelope IsEBB
      CtxtColeRegular  {} -> isEbbEnvelope IsNotEBB

-- | Drop the V1 EBB-or-regular-header envelope and reconstruct the context.
-- Since we don't know the block size, use 'fakeColeBlockSizeHint'.
dropV1Envelope ::
     Lazy.ByteString
  -> Except String (SomeSecond (NestedCtxt Header) ColeBlock, Lazy.ByteString)
dropV1Envelope bs = case Lazy.splitAt 2 bs of
    (prefix, suffix)
      | prefix == isEbbEnvelope IsEBB
      -> return ( SomeSecond . NestedCtxt $ CtxtColeBoundary fakeColeBlockSizeHint
                , suffix
                )
      | prefix == isEbbEnvelope IsNotEBB
      -> return ( SomeSecond . NestedCtxt $ CtxtColeRegular fakeColeBlockSizeHint
                , suffix
                )
      | otherwise
      -> throwError "decodeUnsized: invalid prefix"

-- | Fake size (used in compatibility mode)
fakeColeBlockSizeHint :: SizeInBytes
fakeColeBlockSizeHint = 2000

-- | Encode an unsized header
--
-- Does /not/ have to backwards compatible with legacy (bcc-sl) nodes
-- (which never send or store these headers), but should be inverse to
-- 'decodeSizedHeader', and moreover uses 'fromCBORABlockOrBoundaryHdr' from
-- bcc-ledger-cole, and so we don't have too much choice in this encoder.
encodeUnsizedHeader :: UnsizedHeader -> Encoding
encodeUnsizedHeader (UnsizedHeader raw _ _) = CC.toCBORABlockOrBoundaryHdr raw

-- | Inverse of 'encodeSizedHeader'
decodeUnsizedHeader :: CC.EpochSlots
                    -> Decoder s (Lazy.ByteString -> UnsizedHeader)
decodeUnsizedHeader epochSlots =
    fillInByteString <$> CC.fromCBORABlockOrBoundaryHdr epochSlots
  where
    fillInByteString :: CC.ABlockOrBoundaryHdr ByteSpan
                     -> Lazy.ByteString
                     -> UnsizedHeader
    fillInByteString it theBytes = mkUnsizedHeader epochSlots $
      Lazy.toStrict . slice theBytes <$> it
