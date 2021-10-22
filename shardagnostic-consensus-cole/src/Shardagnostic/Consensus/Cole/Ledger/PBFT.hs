{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- | Instances required to support PBFT
module Shardagnostic.Consensus.Cole.Ledger.PBFT (
    decodeColeChainDepState
  , encodeColeChainDepState
  , fromPBftLedgerView
  , mkColeContextDSIGN
  , toPBftLedgerView
  , toTickedPBftLedgerView
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Data.ByteString (ByteString)

import           Bcc.Binary (Annotated)
import           Bcc.Crypto.DSIGN

import qualified Bcc.Chain.Block as CC
import qualified Bcc.Chain.Delegation as Delegation

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Protocol.Abstract
import           Shardagnostic.Consensus.Protocol.PBFT
import qualified Shardagnostic.Consensus.Protocol.PBFT.State as S

import           Shardagnostic.Consensus.Cole.Crypto.DSIGN
import           Shardagnostic.Consensus.Cole.Ledger.Block
import           Shardagnostic.Consensus.Cole.Ledger.Config
import           Shardagnostic.Consensus.Cole.Ledger.Serialisation ()
import           Shardagnostic.Consensus.Cole.Protocol

type instance BlockProtocol ColeBlock = PBft PBftColeCrypto

-- | Construct DSIGN required for Cole crypto
mkColeContextDSIGN :: BlockConfig  ColeBlock
                    -> VerKeyDSIGN  ColeDSIGN
                    -> ContextDSIGN ColeDSIGN
mkColeContextDSIGN = (,) . coleProtocolMagicId

instance BlockSupportsProtocol ColeBlock where
  validateView cfg hdr@ColeHeader{..} =
      case coleHeaderRaw of
        CC.ABOBBoundaryHdr _    -> pbftValidateBoundary hdr
        CC.ABOBBlockHdr regular ->
          let pbftFields :: PBftFields PBftColeCrypto
                                       (Annotated CC.ToSign ByteString)
              pbftFields = PBftFields {
                  pbftIssuer    = VerKeyColeDSIGN
                                . Delegation.delegateVK
                                . CC.delegationCertificate
                                . CC.headerSignature
                                $ regular
                , pbftGenKey    = VerKeyColeDSIGN
                                . CC.headerGenesisKey
                                $ regular
                , pbftSignature = SignedDSIGN
                                . SigColeDSIGN
                                . CC.signature
                                . CC.headerSignature
                                $ regular
                }

          in PBftValidateRegular
               pbftFields
               (CC.recoverSignedBytes epochSlots regular)
               (mkColeContextDSIGN cfg (pbftGenKey pbftFields))
    where
      epochSlots = coleEpochSlots cfg

  selectView _ = mkPBftSelectView

toPBftLedgerView :: Delegation.Map -> PBftLedgerView PBftColeCrypto
toPBftLedgerView = PBftLedgerView . Delegation.unMap

toTickedPBftLedgerView :: Delegation.Map -> Ticked (PBftLedgerView PBftColeCrypto)
toTickedPBftLedgerView = TickedPBftLedgerView . Delegation.unMap

fromPBftLedgerView :: PBftLedgerView PBftColeCrypto -> Delegation.Map
fromPBftLedgerView = Delegation.Map . pbftDelegates

encodeColeChainDepState
  :: ChainDepState (BlockProtocol ColeBlock)
  -> Encoding
encodeColeChainDepState = S.encodePBftState

decodeColeChainDepState
  :: Decoder s (ChainDepState (BlockProtocol ColeBlock))
decodeColeChainDepState = S.decodePBftState
