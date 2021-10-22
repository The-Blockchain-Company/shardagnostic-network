{-# LANGUAGE NamedFieldPuns #-}
module Shardagnostic.Consensus.Cole.Ledger.Integrity (
    verifyBlockIntegrity
  , verifyHeaderIntegrity
  , verifyHeaderSignature
  ) where

import           Data.Either (isRight)

import qualified Bcc.Chain.Block as CC
import qualified Bcc.Crypto.DSIGN.Class as CC.Crypto

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Protocol.PBFT

import           Shardagnostic.Consensus.Cole.Ledger.Block
import           Shardagnostic.Consensus.Cole.Ledger.Config
import           Shardagnostic.Consensus.Cole.Ledger.PBFT ()

-- | Verify whether a header matches its signature.
--
-- Note that we cannot check this for an EBB, as an EBB contains no signature.
-- This function will always return 'True' for an EBB.
verifyHeaderSignature :: BlockConfig ColeBlock -> Header ColeBlock -> Bool
verifyHeaderSignature cfg hdr =
    case validateView cfg hdr of
      PBftValidateBoundary{} ->
        -- EBB, no signature to check
        True
      PBftValidateRegular fields signed contextDSIGN ->
        let PBftFields { pbftIssuer, pbftSignature } = fields
        in isRight $ CC.Crypto.verifySignedDSIGN
             contextDSIGN
             pbftIssuer
             signed
             pbftSignature

-- | Verify whether a header is not corrupted.
--
-- The difference with 'verifyHeaderSignature' is that this function also
-- checks the integrity of the 'CC.headerProtocolMagicId' field, which is the
-- only field of a regular header that is not signed.
--
-- Note that we cannot check this for an EBB, as an EBB contains no signature.
-- This function will always return 'True' for an EBB.
verifyHeaderIntegrity :: BlockConfig ColeBlock -> Header ColeBlock -> Bool
verifyHeaderIntegrity cfg hdr =
    verifyHeaderSignature cfg hdr &&
    -- @CC.headerProtocolMagicId@ is the only field of a regular header that
    -- is not signed, so check it manually.
    case coleHeaderRaw hdr of
        CC.ABOBBlockHdr h    -> CC.headerProtocolMagicId h == protocolMagicId
        -- EBB, we can't check it
        CC.ABOBBoundaryHdr _ -> True
  where
    protocolMagicId = coleProtocolMagicId cfg

-- | Verifies whether the block is not corrupted by checking its signature and
-- witnesses.
--
-- This function will always return 'True' for an EBB, as we cannot check
-- anything for an EBB.
verifyBlockIntegrity :: BlockConfig ColeBlock -> ColeBlock -> Bool
verifyBlockIntegrity cfg blk =
    verifyHeaderIntegrity cfg hdr &&
    blockMatchesHeader        hdr blk
  where
    hdr = getHeader blk
