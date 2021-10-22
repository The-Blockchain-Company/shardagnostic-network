{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
module Shardagnostic.Consensus.Sophie.Ledger.Block (
    GetHeader (..)
  , Header (..)
  , NestedCtxt_ (..)
  , SophieBasedEra
  , SophieBlock (..)
  , SophieHash (..)
  , mkSophieBlock
  , mkSophieHeader
    -- * Serialisation
  , decodeSophieBlock
  , decodeSophieHeader
  , encodeSophieBlock
  , encodeSophieHeader
  , sophieBinaryBlockInfo
    -- * Conversion
  , fromSophiePrevHash
  , toSophiePrevHash
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Codec.Serialise (Serialise (..))
import qualified Data.ByteString.Lazy as Lazy
import           Data.Coerce (coerce)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))

import           Bcc.Binary (Annotator (..), FromCBOR (..),
                     FullByteString (..), ToCBOR (..), serialize)
import qualified Bcc.Crypto.Hash as Crypto

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.HeaderValidation
import           Shardagnostic.Consensus.Storage.Common (BinaryBlockInfo (..))
import           Shardagnostic.Consensus.Util (ShowProxy (..), hashFromBytesShortE)
import           Shardagnostic.Consensus.Util.Condense

import           Bcc.Ledger.Crypto (Crypto, HASH)
import qualified Bcc.Ledger.Era as SL (hashTxSeq)
import qualified Sophie.Spec.Ledger.API as SL

import           Shardagnostic.Consensus.Sophie.Eras

{-------------------------------------------------------------------------------
  Header hash
-------------------------------------------------------------------------------}

newtype SophieHash c = SophieHash {
      unSophieHash :: SL.HashHeader c
    }
  deriving stock    (Eq, Ord, Show, Generic)
  deriving newtype  (ToCBOR, FromCBOR)
  deriving anyclass (NoThunks)

instance Crypto c => Serialise (SophieHash c) where
  encode = toCBOR
  decode = fromCBOR

instance Condense (SophieHash c) where
  condense = show . unSophieHash

instance SophieBasedEra era => ConvertRawHash (SophieBlock era) where
  toShortRawHash   _ = Crypto.hashToBytesShort . SL.unHashHeader . unSophieHash
  fromShortRawHash _ = SophieHash . SL.HashHeader . hashFromBytesShortE
  hashSize         _ = fromIntegral $ Crypto.sizeHash (Proxy @(HASH (EraCrypto era)))

{-------------------------------------------------------------------------------
  Sophie blocks and headers
-------------------------------------------------------------------------------}

-- | Newtype wrapper to avoid orphan instances
--
-- The phantom type parameter is there to record the additional information
-- we need to work with this block. Most of the code here does not care,
-- but we may need different additional information when running the chain.
data SophieBlock era = SophieBlock {
      sophieBlockRaw        :: !(SL.Block era)
    , sophieBlockHeaderHash :: !(SophieHash (EraCrypto era))
    }

deriving instance SophieBasedEra era => Show (SophieBlock era)
deriving instance SophieBasedEra era => Eq   (SophieBlock era)

instance Typeable era => ShowProxy (SophieBlock era) where

type instance HeaderHash (SophieBlock era) = SophieHash (EraCrypto era)

mkSophieBlock :: SophieBasedEra era => SL.Block era -> SophieBlock era
mkSophieBlock raw = SophieBlock {
      sophieBlockRaw        = raw
    , sophieBlockHeaderHash = SophieHash (SL.bhHash (SL.bheader raw))
    }

data instance Header (SophieBlock era) = SophieHeader {
      sophieHeaderRaw  :: !(SL.BHeader (EraCrypto era))
    , sophieHeaderHash :: !(SophieHash (EraCrypto era))
    }
  deriving (Generic)

deriving instance SophieBasedEra era => Show     (Header (SophieBlock era))
deriving instance SophieBasedEra era => Eq       (Header (SophieBlock era))
deriving instance SophieBasedEra era => NoThunks (Header (SophieBlock era))

instance Typeable era => ShowProxy (Header (SophieBlock era)) where

instance SophieBasedEra era => GetHeader (SophieBlock era) where
  getHeader (SophieBlock rawBlk hdrHash) = SophieHeader {
      sophieHeaderRaw  = SL.bheader rawBlk
    , sophieHeaderHash = hdrHash
    }

  blockMatchesHeader hdr blk =
      -- Compute the hash the body of the block (the transactions) and compare
      -- that against the hash of the body stored in the header.
      SL.hashTxSeq @era txs == SL.bhash hdrBody
    where
      SophieHeader { sophieHeaderRaw = SL.BHeader hdrBody _ } = hdr
      SophieBlock  { sophieBlockRaw  = SL.Block _ txs }       = blk

  headerIsEBB = const Nothing

mkSophieHeader ::
     SophieBasedEra era
  => SL.BHeader (EraCrypto era) -> Header (SophieBlock era)
mkSophieHeader raw = SophieHeader {
      sophieHeaderRaw  = raw
    , sophieHeaderHash = SophieHash (SL.bhHash raw)
    }

instance SophieBasedEra era => HasHeader (SophieBlock era)  where
  getHeaderFields = getBlockHeaderFields

instance SophieBasedEra era => HasHeader (Header (SophieBlock era)) where
  getHeaderFields hdr = HeaderFields {
      headerFieldHash    = sophieHeaderHash hdr
    , headerFieldSlot    =          SL.bheaderSlotNo  . SL.bhbody . sophieHeaderRaw $ hdr
    , headerFieldBlockNo = coerce . SL.bheaderBlockNo . SL.bhbody . sophieHeaderRaw $ hdr
    }

instance SophieBasedEra era => GetPrevHash (SophieBlock era) where
  headerPrevHash =
      fromSophiePrevHash
    . SL.bheaderPrev
    . SL.bhbody
    . sophieHeaderRaw

instance SophieBasedEra era => StandardHash (SophieBlock era)

instance SophieBasedEra era => HasAnnTip (SophieBlock era)

-- The 'ValidateEnvelope' instance lives in the
-- "Shardagnostic.Consensus.Sophie.Ledger.Ledger" module because of the
-- dependency on the 'LedgerConfig'.

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

-- | From @bcc-ledger-specs@ to @shardagnostic-consensus@
fromSophiePrevHash :: SL.PrevHash (EraCrypto era) -> ChainHash (SophieBlock era)
fromSophiePrevHash SL.GenesisHash   = GenesisHash
fromSophiePrevHash (SL.BlockHash h) = BlockHash (SophieHash h)

-- | From @shardagnostic-consensus@ to @bcc-ledger-specs@
toSophiePrevHash :: ChainHash (Header (SophieBlock era)) -> SL.PrevHash (EraCrypto era)
toSophiePrevHash GenesisHash                 = SL.GenesisHash
toSophiePrevHash (BlockHash (SophieHash h)) = SL.BlockHash h

{-------------------------------------------------------------------------------
  NestedCtxt
-------------------------------------------------------------------------------}

data instance NestedCtxt_ (SophieBlock era) f a where
  CtxtSophie :: NestedCtxt_ (SophieBlock era) f (f (SophieBlock era))

deriving instance Show (NestedCtxt_ (SophieBlock era) f a)

instance TrivialDependency (NestedCtxt_ (SophieBlock era) f) where
  type TrivialIndex (NestedCtxt_ (SophieBlock era) f) = f (SophieBlock era)
  hasSingleIndex CtxtSophie CtxtSophie = Refl
  indexIsTrivial = CtxtSophie

instance SameDepIndex (NestedCtxt_ (SophieBlock era) f)
instance HasNestedContent f (SophieBlock era)

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance SophieBasedEra era => ToCBOR (SophieBlock era) where
  -- Don't encode the header hash, we recompute it during deserialisation
  toCBOR = toCBOR . sophieBlockRaw

instance SophieBasedEra era => FromCBOR (Annotator (SophieBlock era)) where
  fromCBOR = fmap mkSophieBlock <$> fromCBOR

instance SophieBasedEra era => ToCBOR (Header (SophieBlock era)) where
  -- Don't encode the header hash, we recompute it during deserialisation
  toCBOR = toCBOR . sophieHeaderRaw

instance SophieBasedEra era => FromCBOR (Annotator (Header (SophieBlock era))) where
  fromCBOR = fmap mkSophieHeader <$> fromCBOR

encodeSophieBlock :: SophieBasedEra era => SophieBlock era -> Encoding
encodeSophieBlock = toCBOR

decodeSophieBlock :: SophieBasedEra era => Decoder s (Lazy.ByteString -> SophieBlock era)
decodeSophieBlock = (. Full) . runAnnotator <$> fromCBOR

sophieBinaryBlockInfo :: SophieBasedEra era => SophieBlock era -> BinaryBlockInfo
sophieBinaryBlockInfo blk = BinaryBlockInfo {
      -- Drop the 'encodeListLen' that precedes the header and the body (= tx
      -- seq)
      headerOffset = 1
      -- The Sophie decoders use annotations, so this is cheap
    , headerSize   = fromIntegral $ Lazy.length (serialize (getHeader blk))
    }

encodeSophieHeader :: SophieBasedEra era => Header (SophieBlock era) -> Encoding
encodeSophieHeader = toCBOR

decodeSophieHeader :: SophieBasedEra era => Decoder s (Lazy.ByteString -> Header (SophieBlock era))
decodeSophieHeader = (. Full) . runAnnotator <$> fromCBOR

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance SophieBasedEra era => Condense (SophieBlock era) where
  condense = show . sophieBlockRaw

instance SophieBasedEra era => Condense (Header (SophieBlock era)) where
  condense = show . sophieHeaderRaw
