{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Shardagnostic.Consensus.ColeDual.Node.Serialisation () where

import qualified Data.ByteString.Lazy as Lazy
import           Data.Proxy

import           Bcc.Chain.Slotting (EpochSlots)

import           Shardagnostic.Network.Block (Serialised, unwrapCBORinCBOR,
                     wrapCBORinCBOR)

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.HeaderValidation
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.Ledger.Dual
import           Shardagnostic.Consensus.Ledger.SupportsMempool (GenTxId)
import           Shardagnostic.Consensus.Node.NetworkProtocolVersion
import           Shardagnostic.Consensus.Node.Run
import           Shardagnostic.Consensus.Node.Serialisation
import           Shardagnostic.Consensus.Protocol.PBFT.State (PBftState)
import           Shardagnostic.Consensus.Storage.Serialisation

import           Shardagnostic.Consensus.Cole.Ledger
import           Shardagnostic.Consensus.Cole.Node.Serialisation ()
import           Shardagnostic.Consensus.Cole.Protocol

import           Shardagnostic.Consensus.ColeSpec.Ledger

import           Shardagnostic.Consensus.ColeDual.Ledger

{-------------------------------------------------------------------------------
  HasNetworkProtocolVersion
-------------------------------------------------------------------------------}

pb :: Proxy ColeBlock
pb = Proxy

instance HasNetworkProtocolVersion DualColeBlock where
  type BlockNodeToNodeVersion   DualColeBlock = BlockNodeToNodeVersion   ColeBlock
  type BlockNodeToClientVersion DualColeBlock = BlockNodeToClientVersion ColeBlock

instance SupportedNetworkProtocolVersion DualColeBlock where
  supportedNodeToNodeVersions     _ = supportedNodeToNodeVersions     pb
  supportedNodeToClientVersions   _ = supportedNodeToClientVersions   pb

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

{-------------------------------------------------------------------------------
  EncodeDisk & DecodeDisk
-------------------------------------------------------------------------------}

instance SerialiseDiskConstraints DualColeBlock

instance EncodeDisk DualColeBlock DualColeBlock where
  encodeDisk _ = encodeDualBlock encodeColeBlock
instance DecodeDisk DualColeBlock (Lazy.ByteString -> DualColeBlock) where
  decodeDisk ccfg = decodeDualBlock (decodeColeBlock epochSlots)
    where
      epochSlots = extractEpochSlots ccfg

instance DecodeDiskDep (NestedCtxt Header) DualColeBlock where
  decodeDiskDep (DualCodecConfig ccfg ColeSpecCodecConfig)
                (NestedCtxt (CtxtDual ctxt)) =
      decodeDiskDep ccfg (NestedCtxt ctxt)

instance EncodeDisk DualColeBlock (LedgerState DualColeBlock) where
  encodeDisk _ = encodeDualLedgerState encodeColeLedgerState
instance DecodeDisk DualColeBlock (LedgerState DualColeBlock) where
  decodeDisk _ = decodeDualLedgerState decodeColeLedgerState

-- | @'ChainDepState' ('BlockProtocol' 'DualColeBlock')@
instance EncodeDisk DualColeBlock (PBftState PBftColeCrypto) where
  encodeDisk _ = encodeColeChainDepState
-- | @'ChainDepState' ('BlockProtocol' 'DualColeBlock')@
instance DecodeDisk DualColeBlock (PBftState PBftColeCrypto) where
  decodeDisk _ = decodeColeChainDepState

instance EncodeDisk DualColeBlock (AnnTip DualColeBlock) where
  encodeDisk ccfg = encodeDisk (dualCodecConfigMain ccfg)
                  . (castAnnTip :: AnnTip DualColeBlock -> AnnTip ColeBlock)
instance DecodeDisk DualColeBlock (AnnTip DualColeBlock) where
  decodeDisk ccfg = (castAnnTip :: AnnTip ColeBlock -> AnnTip DualColeBlock)
                <$> decodeDisk (dualCodecConfigMain ccfg)

{-------------------------------------------------------------------------------
  SerialiseNodeToNode
-------------------------------------------------------------------------------}

instance SerialiseNodeToNodeConstraints DualColeBlock where
  -- We don't enforce this estimate, so we just reuse the estimate from the
  -- concrete header.
  estimateBlockSize = estimateBlockSize . dualHeaderMain

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance SerialiseNodeToNode DualColeBlock DualColeBlock where
  encodeNodeToNode _    _ = wrapCBORinCBOR   (encodeDualBlock  encodeColeBlock)
  decodeNodeToNode ccfg _ = unwrapCBORinCBOR (decodeDualBlock (decodeColeBlock epochSlots))
    where
      epochSlots = extractEpochSlots ccfg

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance SerialiseNodeToNode DualColeBlock (Serialised DualColeBlock)
  -- Default instance

-- | Forward to the Cole instance.
instance SerialiseNodeToNode DualColeBlock (Header DualColeBlock) where
  encodeNodeToNode ccfg version =
        encodeNodeToNode (dualCodecConfigMain ccfg) version
      . dualHeaderMain
  decodeNodeToNode ccfg version =
          DualHeader
      <$> decodeNodeToNode (dualCodecConfigMain ccfg) version

-- | Forward to the Cole instance.
instance SerialiseNodeToNode DualColeBlock (SerialisedHeader DualColeBlock) where
  encodeNodeToNode ccfg version =
        encodeNodeToNode (dualCodecConfigMain ccfg) version
      . dualWrappedMain
  decodeNodeToNode ccfg version =
          rewrapMain
      <$> decodeNodeToNode (dualCodecConfigMain ccfg) version

instance SerialiseNodeToNode DualColeBlock (GenTx DualColeBlock) where
  encodeNodeToNode _ _ = encodeDualGenTx encodeColeGenTx
  decodeNodeToNode _ _ = decodeDualGenTx decodeColeGenTx

instance SerialiseNodeToNode DualColeBlock (GenTxId DualColeBlock) where
  encodeNodeToNode _ _ = encodeDualGenTxId encodeColeGenTxId
  decodeNodeToNode _ _ = decodeDualGenTxId decodeColeGenTxId

{-------------------------------------------------------------------------------
  SerialiseNodeToClient
-------------------------------------------------------------------------------}

instance SerialiseNodeToClientConstraints DualColeBlock

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance SerialiseNodeToClient DualColeBlock DualColeBlock where
  encodeNodeToClient _    _ = wrapCBORinCBOR   (encodeDualBlock  encodeColeBlock)
  decodeNodeToClient ccfg _ = unwrapCBORinCBOR (decodeDualBlock (decodeColeBlock epochSlots))
    where
      epochSlots = extractEpochSlots ccfg

-- | CBOR-in-CBOR for the annotation. This also makes it compatible with the
-- wrapped ('Serialised') variant.
instance SerialiseNodeToClient DualColeBlock (Serialised DualColeBlock)
  -- Default instance

instance SerialiseNodeToClient DualColeBlock (GenTx DualColeBlock) where
  encodeNodeToClient _ _ = encodeDualGenTx encodeColeGenTx
  decodeNodeToClient _ _ = decodeDualGenTx decodeColeGenTx

-- | @'ApplyTxErr' 'DualColeBlock'@
instance SerialiseNodeToClient DualColeBlock (DualGenTxErr ColeBlock ColeSpecBlock) where
  encodeNodeToClient _ _ = encodeDualGenTxErr encodeColeApplyTxError
  decodeNodeToClient _ _ = decodeDualGenTxErr decodeColeApplyTxError

instance SerialiseNodeToClient DualColeBlock (SomeSecond BlockQuery DualColeBlock) where
  encodeNodeToClient _ _ = \case {}
  decodeNodeToClient _ _ = error "DualCole: no query to decode"

instance SerialiseResult DualColeBlock (BlockQuery DualColeBlock) where
  encodeResult _ _ = \case {}
  decodeResult _ _ = \case {}

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

extractEpochSlots :: CodecConfig DualColeBlock -> EpochSlots
extractEpochSlots = getColeEpochSlots . dualCodecConfigMain

-- | The headers for 'DualColeBlock' and 'ColeBlock' are identical, so we
-- can safely cast the serialised forms.
dualWrappedMain :: SerialisedHeader DualColeBlock
                -> SerialisedHeader ColeBlock
dualWrappedMain = castSerialisedHeader ctxtDualMain

-- | Inverse of 'dualWrappedMain'.
rewrapMain :: SerialisedHeader ColeBlock
           -> SerialisedHeader DualColeBlock
rewrapMain = castSerialisedHeader CtxtDual
