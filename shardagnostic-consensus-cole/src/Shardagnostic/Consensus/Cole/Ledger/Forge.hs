{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Shardagnostic.Consensus.Cole.Ledger.Forge (
    forgeColeBlock
  , forgeRegularBlock
    -- * For testing purposes
  , forgeEBB
  ) where

import           Control.Monad (void)
import           Data.ByteString (ByteString)
import           Data.Coerce (coerce)
import           GHC.Stack

import           Bcc.Binary (Annotated (..), reAnnotate)
import qualified Bcc.Chain.Block as CC.Block
import qualified Bcc.Chain.Cole.API as CC
import qualified Bcc.Chain.Common as CC.Common
import qualified Bcc.Chain.Delegation as CC.Delegation
import qualified Bcc.Chain.Genesis as CC.Genesis
import qualified Bcc.Chain.Slotting as CC.Slot
import qualified Bcc.Chain.Ssc as CC.Ssc
import qualified Bcc.Chain.UTxO as CC.UTxO
import qualified Bcc.Chain.Update as CC.Update
import qualified Bcc.Crypto as Crypto
import           Bcc.Crypto.DSIGN

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Config
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.Ledger.SupportsMempool
                     (LedgerSupportsMempool (..), txForgetValidated)
import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits
import           Shardagnostic.Consensus.Protocol.PBFT

import           Shardagnostic.Consensus.Cole.Crypto.DSIGN
import           Shardagnostic.Consensus.Cole.Ledger.Block
import           Shardagnostic.Consensus.Cole.Ledger.Config
import           Shardagnostic.Consensus.Cole.Ledger.Mempool
import           Shardagnostic.Consensus.Cole.Ledger.PBFT
import           Shardagnostic.Consensus.Cole.Protocol

forgeColeBlock
  :: HasCallStack
  => TopLevelConfig ColeBlock
  -> TxLimits.Overrides ColeBlock    -- ^ How to override max tx capacity
                                      --   defined by ledger
  -> BlockNo                          -- ^ Current block number
  -> SlotNo                           -- ^ Current slot number
  -> TickedLedgerState ColeBlock     -- ^ Current ledger
  -> [Validated (GenTx ColeBlock)]   -- ^ Txs to consider adding in the block
  -> PBftIsLeader PBftColeCrypto     -- ^ Leader proof ('IsLeader')
  -> ColeBlock
forgeColeBlock cfg = forgeRegularBlock (configBlock cfg)

forgeEBB
  :: BlockConfig ColeBlock
  -> SlotNo                          -- ^ Current slot
  -> BlockNo                         -- ^ Current block number
  -> ChainHash ColeBlock            -- ^ Previous hash
  -> ColeBlock
forgeEBB cfg curSlot curNo prevHash =
        mkColeBlock epochSlots
      . CC.Block.ABOBBoundary
      . CC.reAnnotateBoundary (coleProtocolMagicId cfg)
      $ boundaryBlock
  where
    epochSlots :: CC.Slot.EpochSlots
    epochSlots = coleEpochSlots cfg

    prevHeaderHash :: Either CC.Genesis.GenesisHash CC.Block.HeaderHash
    prevHeaderHash = case prevHash of
      GenesisHash             -> Left  (coleGenesisHash cfg)
      BlockHash (ColeHash h) -> Right h

    boundaryBlock :: CC.Block.ABoundaryBlock ()
    boundaryBlock =
      CC.Block.ABoundaryBlock {
        CC.Block.boundaryBlockLength = 0 -- Used only in testing anyway
      , CC.Block.boundaryHeader
      , CC.Block.boundaryBody        = CC.Block.ABoundaryBody ()
      , CC.Block.boundaryAnnotation  = ()
      }

    boundaryHeader :: CC.Block.ABoundaryHeader ()
    boundaryHeader = CC.Block.mkABoundaryHeader
      prevHeaderHash
      epoch
      (coerce curNo)
      ()
      where
        CC.Slot.EpochNumber epoch =
          CC.Slot.slotNumberEpoch epochSlots (coerce curSlot)

-- | Internal helper data type for 'forgeRegularBlock' used to accumulate the
-- different kinds of block payloads that can be found in a given collection
-- of Cole 'GenTx's.
--
-- n.b. This data type is not to be exposed from this module.
data BlockPayloads = BlockPayloads
  { bpTxs        :: ![CC.UTxO.TxAux]
  , bpDlgCerts   :: ![CC.Delegation.Certificate]
  , bpUpVotes    :: ![CC.Update.Vote]
  , bpUpProposal :: !(Maybe CC.Update.Proposal)
    -- ^ 'Just' if there is at least one 'CC.Update.Proposal' in a list of
    -- Cole 'GenTx's and 'Nothing' if there are none. It is worth noting that
    -- if we encounter multiple 'CC.Update.Proposal's in a collection of
    -- 'GenTx's, this value will be that of the last 'CC.Update.Proposal'
    -- encountered.
  }

initBlockPayloads :: BlockPayloads
initBlockPayloads = BlockPayloads
  { bpTxs        = []
  , bpDlgCerts   = []
  , bpUpVotes    = []
  , bpUpProposal = Nothing
  }

forgeRegularBlock
  :: HasCallStack
  => BlockConfig ColeBlock
  -> TxLimits.Overrides ColeBlock     -- ^ How to override max tx capacity
                                       --   defined by ledger
  -> BlockNo                           -- ^ Current block number
  -> SlotNo                            -- ^ Current slot number
  -> TickedLedgerState ColeBlock      -- ^ Current ledger
  -> [Validated (GenTx ColeBlock)]    -- ^ Txs to consider adding in the block
  -> PBftIsLeader PBftColeCrypto      -- ^ Leader proof ('IsLeader')
  -> ColeBlock
forgeRegularBlock cfg maxTxCapacityOverrides bno sno st txs isLeader =
    forge $
      forgePBftFields
        (mkColeContextDSIGN cfg)
        isLeader
        (reAnnotate $ Annotated toSign ())
  where
    epochSlots :: CC.Slot.EpochSlots
    epochSlots = coleEpochSlots cfg

    blockPayloads :: BlockPayloads
    blockPayloads =
        foldr
          extendBlockPayloads
          initBlockPayloads
          (takeLargestPrefixThatFits maxTxCapacityOverrides st txs)

    txPayload :: CC.UTxO.TxPayload
    txPayload = CC.UTxO.mkTxPayload (bpTxs blockPayloads)

    dlgPayload :: CC.Delegation.Payload
    dlgPayload = CC.Delegation.unsafePayload (bpDlgCerts blockPayloads)

    updatePayload :: CC.Update.Payload
    updatePayload = CC.Update.payload (bpUpProposal blockPayloads)
                                      (bpUpVotes blockPayloads)

    extendBlockPayloads :: Validated (GenTx ColeBlock)
                        -> BlockPayloads
                        -> BlockPayloads
    extendBlockPayloads validatedGenTx bp@BlockPayloads{bpTxs, bpDlgCerts, bpUpVotes} =
      -- TODO: We should try to use 'recoverProof' (and other variants of
      -- 'recoverBytes') here as opposed to throwing away the serializations
      -- (the 'ByteString' annotations) with 'void' as we're currently doing.
      case txForgetValidated validatedGenTx of
        ColeTx             _ tx   -> bp { bpTxs        = void tx : bpTxs }
        ColeDlg            _ cert -> bp { bpDlgCerts   = void cert : bpDlgCerts }
        -- TODO: We should throw an error if we encounter multiple
        -- 'ColeUpdateProposal's (i.e. if 'bpUpProposal' 'isJust').
        -- This is because we should only be provided with a maximum of one
        -- 'ColeUpdateProposal' to include in a block payload.
        ColeUpdateProposal _ prop -> bp { bpUpProposal = Just (void prop) }
        ColeUpdateVote     _ vote -> bp { bpUpVotes    = void vote : bpUpVotes }

    body :: CC.Block.Body
    body = CC.Block.ABody {
          CC.Block.bodyTxPayload     = txPayload
        , CC.Block.bodySscPayload    = CC.Ssc.SscPayload
        , CC.Block.bodyDlgPayload    = dlgPayload
        , CC.Block.bodyUpdatePayload = updatePayload
        }

    proof :: CC.Block.Proof
    proof = CC.Block.mkProof body

    prevHeaderHash :: CC.Block.HeaderHash
    prevHeaderHash = case getTipHash st of
      GenesisHash             -> error
        "the first block on the Cole chain must be an EBB"
      BlockHash (ColeHash h) -> h

    epochAndSlotCount :: CC.Slot.EpochAndSlotCount
    epochAndSlotCount = CC.Slot.fromSlotNumber epochSlots (coerce sno)

    toSign :: CC.Block.ToSign
    toSign = CC.Block.ToSign {
          CC.Block.tsHeaderHash      = prevHeaderHash
        , CC.Block.tsSlot            = epochAndSlotCount
        , CC.Block.tsDifficulty      = coerce bno
        , CC.Block.tsBodyProof       = proof
        , CC.Block.tsProtocolVersion = coleProtocolVersion cfg
        , CC.Block.tsSoftwareVersion = coleSoftwareVersion cfg
        }

    dlgCertificate :: CC.Delegation.Certificate
    dlgCertificate = pbftIsLeaderDlgCert isLeader

    headerGenesisKey :: Crypto.VerificationKey
    VerKeyColeDSIGN headerGenesisKey = dlgCertGenVerKey dlgCertificate

    forge :: PBftFields PBftColeCrypto (Annotated CC.Block.ToSign ByteString)
          -> ColeBlock
    forge arkPayload = annotateColeBlock epochSlots block
      where
        block :: CC.Block.Block
        block = CC.Block.ABlock {
              CC.Block.blockHeader     = header
            , CC.Block.blockBody       = body
            , CC.Block.blockAnnotation = ()
            }

        headerSignature :: CC.Block.BlockSignature
        headerSignature = CC.Block.ABlockSignature dlgCertificate (coerce sig)
          where
            sig :: Crypto.Signature CC.Block.ToSign
            SignedDSIGN (SigColeDSIGN sig) = pbftSignature arkPayload

        header :: CC.Block.Header
        header = CC.Block.AHeader {
              CC.Block.aHeaderProtocolMagicId = ann (Crypto.getProtocolMagicId (coleProtocolMagic cfg))
            , CC.Block.aHeaderPrevHash        = ann prevHeaderHash
            , CC.Block.aHeaderSlot            = ann (coerce sno)
            , CC.Block.aHeaderDifficulty      = ann (coerce bno)
            , CC.Block.headerProtocolVersion  = coleProtocolVersion cfg
            , CC.Block.headerSoftwareVersion  = coleSoftwareVersion cfg
            , CC.Block.aHeaderProof           = ann proof
            , CC.Block.headerGenesisKey       = headerGenesisKey
            , CC.Block.headerSignature        = headerSignature
            , CC.Block.headerAnnotation       = ()
            , CC.Block.headerExtraAnnotation  = ()
            }

        ann :: b -> Annotated b ()
        ann b = Annotated b ()
