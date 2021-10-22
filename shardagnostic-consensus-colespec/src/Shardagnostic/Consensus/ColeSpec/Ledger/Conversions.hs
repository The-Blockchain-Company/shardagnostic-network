-- | Conversions from shardagnostic-consensus types to the Cole spec types
--
-- Intended for unqualified import.
module Shardagnostic.Consensus.ColeSpec.Ledger.Conversions (
    -- * Spec to consensus
    fromColeSpecPrevHash
  , fromColeSpecSlotNo
    -- * Consensus to spec
  , toColeSpecSlotNo
  ) where

import           Shardagnostic.Consensus.Block

import qualified Cole.Spec.Chain.STS.Block as Spec
import qualified Cole.Spec.Ledger.Core as Spec

{-------------------------------------------------------------------------------
  Spec to consensus
-------------------------------------------------------------------------------}

fromColeSpecPrevHash :: (Spec.Hash -> HeaderHash b)
                      -> Spec.Hash -> ChainHash b
fromColeSpecPrevHash f h
  | h == Spec.genesisHash = GenesisHash
  | otherwise             = BlockHash (f h)

fromColeSpecSlotNo :: Spec.Slot -> SlotNo
fromColeSpecSlotNo (Spec.Slot slot) = SlotNo slot

{-------------------------------------------------------------------------------
  Consensus to spec
-------------------------------------------------------------------------------}

toColeSpecSlotNo :: SlotNo -> Spec.Slot
toColeSpecSlotNo (SlotNo slot) = Spec.Slot slot
