{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE TypeFamilies             #-}
module Shardagnostic.Consensus.Sophie.Ledger.Integrity (
    verifyBlockIntegrity
  , verifyHeaderIntegrity
  ) where

import           Data.Either (isRight)
import           Data.Word (Word64)

import           Shardagnostic.Consensus.Block

import qualified Bcc.Ledger.Keys as SL (verifySignedKES)
import qualified Sophie.Spec.Ledger.API as SL

import           Shardagnostic.Consensus.Sophie.Ledger.Block

-- | Verify whether a header is not corrupted
verifyHeaderIntegrity ::
     SophieBasedEra era
  => Word64  -- ^ 'toptimumSlotsPerKESPeriod'
  -> Header (SophieBlock era)
  -> Bool
verifyHeaderIntegrity slotsPerKESPeriod hdr@SophieHeader { sophieHeaderRaw } =
    isRight $ SL.verifySignedKES () ocertVkHot t hdrBody hdrSignature
  where
    SL.BHeader hdrBody hdrSignature = sophieHeaderRaw
    SL.OCert {
        ocertVkHot
      , ocertKESPeriod = SL.KESPeriod startOfKesPeriod
      } = SL.bheaderOCert hdrBody

    currentKesPeriod = fromIntegral $
      unSlotNo (blockSlot hdr) `div` slotsPerKESPeriod

    t | currentKesPeriod >= startOfKesPeriod
      = currentKesPeriod - startOfKesPeriod
      | otherwise
      = 0

-- | Verifies whether the block is not corrupted by checking its signature and
-- witnesses
verifyBlockIntegrity ::
     SophieBasedEra era
  => Word64  -- ^ 'toptimumSlotsPerKESPeriod'
  -> SophieBlock era -> Bool
verifyBlockIntegrity slotsPerKESPeriod blk =
    verifyHeaderIntegrity slotsPerKESPeriod (getHeader blk) &&
    blockMatchesHeader                      (getHeader blk) blk
