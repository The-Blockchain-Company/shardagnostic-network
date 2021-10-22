{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Shardagnostic.Consensus.Sophie.Node.Serialisation () where

import           Control.Exception (Exception, throw)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Typeable (Typeable)

import           Bcc.Binary (fromCBOR, toCBOR)
import           Codec.Serialise (decode, encode)

import           Shardagnostic.Network.Block (Serialised, unwrapCBORinCBOR,
                     wrapCBORinCBOR)

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.HeaderValidation
import           Shardagnostic.Consensus.Ledger.SupportsMempool (GenTxId)
import           Shardagnostic.Consensus.Node.Run
import           Shardagnostic.Consensus.Node.Serialisation
import           Shardagnostic.Consensus.Storage.Serialisation

import qualified Sophie.Spec.Ledger.API as SL

import           Shardagnostic.Consensus.Sophie.Eras
import           Shardagnostic.Consensus.Sophie.Ledger
import           Shardagnostic.Consensus.Sophie.Ledger.NetworkProtocolVersion ()
import           Shardagnostic.Consensus.Sophie.Protocol

{-------------------------------------------------------------------------------
  EncodeDisk & DecodeDisk
-------------------------------------------------------------------------------}

instance SophieBasedEra era => HasBinaryBlockInfo (SophieBlock era) where
  getBinaryBlockInfo = sophieBinaryBlockInfo

instance SophieBasedEra era => SerialiseDiskConstraints (SophieBlock era)

instance SophieBasedEra era => EncodeDisk (SophieBlock era) (SophieBlock era) where
  encodeDisk _ = encodeSophieBlock
instance SophieBasedEra era => DecodeDisk (SophieBlock era) (Lazy.ByteString -> SophieBlock era) where
  decodeDisk _ = decodeSophieBlock

instance SophieBasedEra era => EncodeDisk (SophieBlock era) (Header (SophieBlock era)) where
  encodeDisk _ = encodeSophieHeader
instance SophieBasedEra era => DecodeDisk (SophieBlock era) (Lazy.ByteString -> Header (SophieBlock era)) where
  decodeDisk _ = decodeSophieHeader

instance SophieBasedEra era => EncodeDisk (SophieBlock era) (LedgerState (SophieBlock era)) where
  encodeDisk _ = encodeSophieLedgerState
instance SophieBasedEra era => DecodeDisk (SophieBlock era) (LedgerState (SophieBlock era)) where
  decodeDisk _ = decodeSophieLedgerState

-- | @'ChainDepState' ('BlockProtocol' ('SophieBlock' era))@
instance (SophieBasedEra era, EraCrypto era ~ c) => EncodeDisk (SophieBlock era) (TOptimumState c) where
  encodeDisk _ = encode
-- | @'ChainDepState' ('BlockProtocol' ('SophieBlock' era))@
instance (SophieBasedEra era, EraCrypto era ~ c) => DecodeDisk (SophieBlock era) (TOptimumState c) where
  decodeDisk _ = decode

instance SophieBasedEra era => EncodeDisk (SophieBlock era) (AnnTip (SophieBlock era)) where
  encodeDisk _ = encodeSophieAnnTip
instance SophieBasedEra era =>  DecodeDisk (SophieBlock era) (AnnTip (SophieBlock era)) where
  decodeDisk _ = decodeSophieAnnTip

{-------------------------------------------------------------------------------
  SerialiseNodeToNode
-------------------------------------------------------------------------------}

instance SophieBasedEra era => SerialiseNodeToNodeConstraints (SophieBlock era) where
  estimateBlockSize hdr = overhead + hdrSize + bodySize
    where
      -- The maximum block size is 65536, the CBOR-in-CBOR tag for this block
      -- is:
      --
      -- > D8 18          # tag(24)
      -- >    1A 00010000 # bytes(65536)
      --
      -- Which is 7 bytes, enough for up to 4294967295 bytes.
      overhead = 7 {- CBOR-in-CBOR -} + 1 {- encodeListLen -}
      bodySize = fromIntegral . SL.bsize . SL.bhbody . sophieHeaderRaw $ hdr
      hdrSize  = fromIntegral . SL.bHeaderSize . sophieHeaderRaw $ hdr

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance SophieBasedEra era => SerialiseNodeToNode (SophieBlock era) (SophieBlock era) where
  encodeNodeToNode _ _ = wrapCBORinCBOR   encodeSophieBlock
  decodeNodeToNode _ _ = unwrapCBORinCBOR decodeSophieBlock

-- | 'Serialised' uses CBOR-in-CBOR by default.
instance SerialiseNodeToNode (SophieBlock era) (Serialised (SophieBlock era))
  -- Default instance

-- | CBOR-in-CBOR to be compatible with the wrapped ('Serialised') variant.
instance SophieBasedEra era => SerialiseNodeToNode (SophieBlock era) (Header (SophieBlock era)) where
  encodeNodeToNode _ _ = wrapCBORinCBOR   encodeSophieHeader
  decodeNodeToNode _ _ = unwrapCBORinCBOR decodeSophieHeader

-- | We use CBOR-in-CBOR
instance SerialiseNodeToNode (SophieBlock era) (SerialisedHeader (SophieBlock era)) where
  encodeNodeToNode _ _ = encodeTrivialSerialisedHeader
  decodeNodeToNode _ _ = decodeTrivialSerialisedHeader

-- | The @To/FromCBOR@ instances defined in @bcc-ledger-specs@ use
-- CBOR-in-CBOR to get the annotation.
instance SophieBasedEra era => SerialiseNodeToNode (SophieBlock era) (GenTx (SophieBlock era)) where
  encodeNodeToNode _ _ = toCBOR
  decodeNodeToNode _ _ = fromCBOR

instance SophieBasedEra era => SerialiseNodeToNode (SophieBlock era) (GenTxId (SophieBlock era)) where
  encodeNodeToNode _ _ = toCBOR
  decodeNodeToNode _ _ = fromCBOR

{-------------------------------------------------------------------------------
  SerialiseNodeToClient
-------------------------------------------------------------------------------}

-- | Exception thrown in the encoders
data SophieEncoderException era =
    -- | A query was submitted that is not supported by the given
    -- 'SophieNodeToClientVersion'.
    SophieEncoderUnsupportedQuery
         (SomeSecond BlockQuery (SophieBlock era))
         SophieNodeToClientVersion
  deriving (Show)

instance Typeable era => Exception (SophieEncoderException era)

instance SophieBasedEra era => SerialiseNodeToClientConstraints (SophieBlock era)

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance SophieBasedEra era => SerialiseNodeToClient (SophieBlock era) (SophieBlock era) where
  encodeNodeToClient _ _ = wrapCBORinCBOR   encodeSophieBlock
  decodeNodeToClient _ _ = unwrapCBORinCBOR decodeSophieBlock

-- | 'Serialised' uses CBOR-in-CBOR by default.
instance SerialiseNodeToClient (SophieBlock era) (Serialised (SophieBlock era))
  -- Default instance

-- | Uses CBOR-in-CBOR in the @To/FromCBOR@ instances to get the annotation.
instance SophieBasedEra era => SerialiseNodeToClient (SophieBlock era) (GenTx (SophieBlock era)) where
  encodeNodeToClient _ _ = toCBOR
  decodeNodeToClient _ _ = fromCBOR

-- | @'ApplyTxErr' '(SophieBlock era)'@
instance SophieBasedEra era => SerialiseNodeToClient (SophieBlock era) (SL.ApplyTxError era) where
  encodeNodeToClient _ _ = toCBOR
  decodeNodeToClient _ _ = fromCBOR

instance SophieBasedEra era
      => SerialiseNodeToClient (SophieBlock era) (SomeSecond BlockQuery (SophieBlock era)) where
  encodeNodeToClient _ version (SomeSecond q)
    | querySupportedVersion q version
    = encodeSophieQuery q
    | otherwise
    = throw $ SophieEncoderUnsupportedQuery (SomeSecond q) version
  decodeNodeToClient _ _ = decodeSophieQuery

instance SophieBasedEra era => SerialiseResult (SophieBlock era) (BlockQuery (SophieBlock era)) where
  encodeResult _ _ = encodeSophieResult
  decodeResult _ _ = decodeSophieResult

{-------------------------------------------------------------------------------
  HFC support

  Since 'NestedCtxt' for Sophie is trivial, these instances can use defaults.
-------------------------------------------------------------------------------}

instance SophieBasedEra era => ReconstructNestedCtxt Header (SophieBlock era)
instance SophieBasedEra era => EncodeDiskDepIx (NestedCtxt Header) (SophieBlock era)
instance SophieBasedEra era => EncodeDiskDep   (NestedCtxt Header) (SophieBlock era)
instance SophieBasedEra era => DecodeDiskDepIx (NestedCtxt Header) (SophieBlock era)
instance SophieBasedEra era => DecodeDiskDep   (NestedCtxt Header) (SophieBlock era)
