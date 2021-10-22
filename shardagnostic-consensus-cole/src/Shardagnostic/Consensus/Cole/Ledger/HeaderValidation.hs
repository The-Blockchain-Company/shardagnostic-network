{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Shardagnostic.Consensus.Cole.Ledger.HeaderValidation (
    ColeOtherHeaderEnvelopeError (..)
  , TipInfoIsEBB (..)
  ) where

import           Control.Monad.Except
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import qualified Bcc.Chain.Slotting as CC

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Config
import           Shardagnostic.Consensus.HeaderValidation

import           Shardagnostic.Consensus.Cole.Ledger.Block
import           Shardagnostic.Consensus.Cole.Ledger.Config
import           Shardagnostic.Consensus.Cole.Ledger.Orphans ()
import           Shardagnostic.Consensus.Cole.Ledger.PBFT ()

{-------------------------------------------------------------------------------
  Envelope
-------------------------------------------------------------------------------}

instance HasAnnTip ColeBlock where
  type TipInfo ColeBlock = TipInfoIsEBB ColeBlock
  tipInfoHash _ (TipInfoIsEBB h _) = h
  getTipInfo b = TipInfoIsEBB (blockHash b) (coleHeaderIsEBB b)

data ColeOtherHeaderEnvelopeError =
    UnexpectedEBBInSlot !SlotNo
  deriving (Eq, Show, Generic, NoThunks)

instance BasicEnvelopeValidation ColeBlock where
  expectedFirstBlockNo  _ = BlockNo 0
  minimumPossibleSlotNo _ = SlotNo 0

  -- EBB shares its block number with its predecessor
  expectedNextBlockNo _ (TipInfoIsEBB _ prevIsEBB) (TipInfoIsEBB _ curIsEBB) b =
     case (prevIsEBB, curIsEBB) of
       (IsNotEBB, IsEBB) -> b
       _otherwise        -> succ b

  -- EBB shares its slot number with its successor
  minimumNextSlotNo _ (TipInfoIsEBB _ prevIsEBB) (TipInfoIsEBB _ curIsEBB) s =
      case (prevIsEBB, curIsEBB) of
        (IsEBB, IsNotEBB) -> s
        _otherwise        -> succ s

instance ValidateEnvelope ColeBlock where
  type OtherHeaderEnvelopeError ColeBlock = ColeOtherHeaderEnvelopeError

  additionalEnvelopeChecks cfg _ledgerView hdr =
      when (fromIsEBB newIsEBB && not (canBeEBB actualSlotNo)) $
        throwError $ UnexpectedEBBInSlot actualSlotNo
    where
      actualSlotNo :: SlotNo
      actualSlotNo = blockSlot hdr

      newIsEBB :: IsEBB
      newIsEBB = coleHeaderIsEBB hdr

      canBeEBB :: SlotNo -> Bool
      canBeEBB (SlotNo s) = s `mod` epochSlots == 0

      epochSlots :: Word64
      epochSlots =
          CC.unEpochSlots
        . coleEpochSlots
        . configBlock
        $ cfg
