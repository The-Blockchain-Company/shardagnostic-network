{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Shardagnostic.Consensus.Cole.Node.Serialisation () where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (decode, encode)
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Short as Short

import           Bcc.Binary
import           Bcc.Prelude (cborError)

import qualified Bcc.Chain.Block as CC
import qualified Bcc.Chain.Cole.API as CC

import           Shardagnostic.Network.Block (Serialised (..), unwrapCBORinCBOR,
                     wrapCBORinCBOR)

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.HeaderValidation
import           Shardagnostic.Consensus.Ledger.SupportsMempool (GenTxId)
import           Shardagnostic.Consensus.Node.Run
import           Shardagnostic.Consensus.Node.Serialisation
import           Shardagnostic.Consensus.Protocol.PBFT.State (PBftState)
import           Shardagnostic.Consensus.Storage.Serialisation

import           Shardagnostic.Consensus.Cole.Ledger
import           Shardagnostic.Consensus.Cole.Ledger.Conversions
import           Shardagnostic.Consensus.Cole.Protocol

{-------------------------------------------------------------------------------
  EncodeDisk & DecodeDisk
-------------------------------------------------------------------------------}

instance HasBinaryBlockInfo ColeBlock where
  getBinaryBlockInfo = coleBinaryBlockInfo

instance SerialiseDiskConstraints ColeBlock

instance EncodeDisk ColeBlock ColeBlock where
  encodeDisk _ = encodeColeBlock
instance DecodeDisk ColeBlock (Lazy.ByteString -> ColeBlock) where
  decodeDisk ccfg = decodeColeBlock (getColeEpochSlots ccfg)

instance EncodeDisk ColeBlock (LedgerState ColeBlock) where
  encodeDisk _ = encodeColeLedgerState
instance DecodeDisk ColeBlock (LedgerState ColeBlock) where
  decodeDisk _ = decodeColeLedgerState

-- | @'ChainDepState' ('BlockProtocol' 'ColeBlock')@
instance EncodeDisk ColeBlock (PBftState PBftColeCrypto) where
  encodeDisk _ = encodeColeChainDepState
-- | @'ChainDepState' ('BlockProtocol' 'ColeBlock')@
instance DecodeDisk ColeBlock (PBftState PBftColeCrypto) where
  decodeDisk _ = decodeColeChainDepState

instance EncodeDisk ColeBlock (AnnTip ColeBlock) where
  encodeDisk _ = encodeColeAnnTip
instance DecodeDisk ColeBlock (AnnTip ColeBlock) where
  decodeDisk _ = decodeColeAnnTip

{-------------------------------------------------------------------------------
  SerialiseNodeToNode
-------------------------------------------------------------------------------}

instance SerialiseNodeToNodeConstraints ColeBlock where
  estimateBlockSize = coleHeaderBlockSizeHint

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance SerialiseNodeToNode ColeBlock ColeBlock where
  encodeNodeToNode _    _ = wrapCBORinCBOR    encodeColeBlock
  decodeNodeToNode ccfg _ = unwrapCBORinCBOR (decodeColeBlock epochSlots)
    where
      epochSlots = getColeEpochSlots ccfg

instance SerialiseNodeToNode ColeBlock (Header ColeBlock) where
  encodeNodeToNode ccfg = \case
      ColeNodeToNodeVersion1 ->
        wrapCBORinCBOR $
          encodeUnsizedHeader . fst . splitSizeHint
      ColeNodeToNodeVersion2 ->
        encodeDisk ccfg . unnest

  decodeNodeToNode ccfg = \case
      ColeNodeToNodeVersion1 ->
        unwrapCBORinCBOR $
              (flip joinSizeHint fakeColeBlockSizeHint .)
          <$> decodeUnsizedHeader epochSlots
      ColeNodeToNodeVersion2 ->
        nest <$> decodeDisk ccfg
    where
      epochSlots = getColeEpochSlots ccfg

-- | 'Serialised' uses CBOR-in-CBOR by default.
instance SerialiseNodeToNode ColeBlock (Serialised ColeBlock)
  -- Default instance

instance SerialiseNodeToNode ColeBlock (SerialisedHeader ColeBlock) where
  encodeNodeToNode ccfg version = case version of
      -- Drop the context and add the tag, encode that using CBOR-in-CBOR
      ColeNodeToNodeVersion1 ->
            encode
          . Serialised
          . addV1Envelope
          . aux
          . serialisedHeaderToDepPair
        where
          aux :: GenDepPair Serialised (f blk)
              -> (SomeSecond f blk, Lazy.ByteString)
          aux (GenDepPair ix (Serialised bytes)) = (SomeSecond ix, bytes)

      ColeNodeToNodeVersion2 -> encodeDisk ccfg

  decodeNodeToNode ccfg version = case version of
      ColeNodeToNodeVersion1 -> do
          bs <- unSerialised <$> decode
          either fail (return . SerialisedHeaderFromDepPair) $
            runExcept $ aux <$> dropV1Envelope bs
        where
          aux :: (SomeSecond f blk, Lazy.ByteString)
              -> GenDepPair Serialised (f blk)
          aux (SomeSecond ix, bytes) = GenDepPair ix (Serialised bytes)

      ColeNodeToNodeVersion2 -> decodeDisk ccfg

-- | No CBOR-in-CBOR, because we check for canonical encodings, which means we
-- can use the recomputed encoding for the annotation.
instance SerialiseNodeToNode ColeBlock (GenTx ColeBlock) where
  encodeNodeToNode _ _ = encodeColeGenTx
  decodeNodeToNode _ _ = decodeColeGenTx

instance SerialiseNodeToNode ColeBlock (GenTxId ColeBlock) where
  encodeNodeToNode _ _ = encodeColeGenTxId
  decodeNodeToNode _ _ = decodeColeGenTxId

{-------------------------------------------------------------------------------
  SerialiseNodeToClient
-------------------------------------------------------------------------------}

instance SerialiseNodeToClientConstraints ColeBlock

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance SerialiseNodeToClient ColeBlock ColeBlock where
  encodeNodeToClient _    _ = wrapCBORinCBOR    encodeColeBlock
  decodeNodeToClient ccfg _ = unwrapCBORinCBOR (decodeColeBlock epochSlots)
    where
      epochSlots = getColeEpochSlots ccfg

-- | 'Serialised' uses CBOR-in-CBOR by default.
instance SerialiseNodeToClient ColeBlock (Serialised ColeBlock)
  -- Default instance

-- | No CBOR-in-CBOR, because we check for canonical encodings, which means we
-- can use the recomputed encoding for the annotation.
instance SerialiseNodeToClient ColeBlock (GenTx ColeBlock) where
  encodeNodeToClient _ _ = encodeColeGenTx
  decodeNodeToClient _ _ = decodeColeGenTx

-- | @'ApplyTxErr' 'ColeBlock'@
instance SerialiseNodeToClient ColeBlock CC.ApplyMempoolPayloadErr where
  encodeNodeToClient _ _ = encodeColeApplyTxError
  decodeNodeToClient _ _ = decodeColeApplyTxError

instance SerialiseNodeToClient ColeBlock (SomeSecond BlockQuery ColeBlock) where
  encodeNodeToClient _ _ (SomeSecond q) = encodeColeQuery q
  decodeNodeToClient _ _               = decodeColeQuery

instance SerialiseResult ColeBlock (BlockQuery ColeBlock) where
  encodeResult _ _ = encodeColeResult
  decodeResult _ _ = decodeColeResult

{-------------------------------------------------------------------------------
  Nested contents
-------------------------------------------------------------------------------}

instance ReconstructNestedCtxt Header ColeBlock where
  reconstructPrefixLen _ = PrefixLen 2
  reconstructNestedCtxt _proxy prefix size =
      -- The first byte is @encodeListLen 2@, the second (index 1) is 0 for
      -- EBB, 1 for regular block
      case Short.index prefix 1 of
        0 -> SomeSecond $ NestedCtxt (CtxtColeBoundary size)
        1 -> SomeSecond $ NestedCtxt (CtxtColeRegular  size)
        _ -> error $ "invalid ColeBlock with prefix: " <> show prefix

instance EncodeDiskDepIx (NestedCtxt Header) ColeBlock where
  encodeDiskDepIx _ccfg (SomeSecond (NestedCtxt ctxt)) = mconcat [
        CBOR.encodeListLen 2
      , case ctxt of
          CtxtColeBoundary size -> mconcat [
              CBOR.encodeWord8 0
            , CBOR.encodeWord32 size
            ]
          CtxtColeRegular size -> mconcat [
              CBOR.encodeWord8 1
            , CBOR.encodeWord32 size
            ]
      ]

instance EncodeDiskDep (NestedCtxt Header) ColeBlock where
  encodeDiskDep _ccfg (NestedCtxt ctxt) h =
      case ctxt of
        CtxtColeRegular _size ->
          encodeColeRegularHeader h
        CtxtColeBoundary _size ->
          -- We don't encode the 'SlotNo'
          -- This is important, because this encoder/decoder must be compatible
          -- with the raw bytes as stored on disk as part of a Cole block.
          encodeColeBoundaryHeader (snd h)

instance DecodeDiskDepIx (NestedCtxt Header) ColeBlock where
  decodeDiskDepIx _ccfg = do
      enforceSize "decodeDiskDepIx ColeBlock" 2
      CBOR.decodeWord8 >>= \case
        0 -> SomeSecond . NestedCtxt . CtxtColeBoundary <$> CBOR.decodeWord32
        1 -> SomeSecond . NestedCtxt . CtxtColeRegular  <$> CBOR.decodeWord32
        t -> cborError $ DecoderErrorUnknownTag "decodeDiskDepIx ColeBlock" t

instance DecodeDiskDep (NestedCtxt Header) ColeBlock where
  decodeDiskDep ColeCodecConfig{..} (NestedCtxt ctxt) =
      case ctxt of
        CtxtColeRegular _size ->
          decodeColeRegularHeader getColeEpochSlots
        CtxtColeBoundary _size ->
          auxBoundary <$> decodeColeBoundaryHeader
    where
      auxBoundary :: (Lazy.ByteString -> RawBoundaryHeader)
                  -> (Lazy.ByteString -> (SlotNo, RawBoundaryHeader))
      auxBoundary f bs =
          (slotNo, hdr)
        where
          hdr :: RawBoundaryHeader
          hdr = f bs

          slotNo :: SlotNo
          slotNo = fromColeSlotNo $
              CC.boundaryBlockSlot getColeEpochSlots (CC.boundaryEpoch hdr)
