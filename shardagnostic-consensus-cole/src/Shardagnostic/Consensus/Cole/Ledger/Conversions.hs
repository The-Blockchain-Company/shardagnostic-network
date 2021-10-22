module Shardagnostic.Consensus.Cole.Ledger.Conversions (
    -- * From @bcc-ledger-cole@ to @shardagnostic-consensus@
    fromColeBlockCount
  , fromColeBlockNo
  , fromColeEpochSlots
  , fromColeSlotLength
  , fromColeSlotNo
    -- * From @shardagnostic-consensus@ to @bcc-ledger-cole@
  , toColeBlockCount
  , toColeSlotLength
  , toColeSlotNo
    -- * Extract info from the genesis config
  , genesisNumCoreNodes
  , genesisSecurityParam
  , genesisSlotLength
  ) where

import           Data.Coerce
import qualified Data.Set as Set
import           Numeric.Natural (Natural)

import qualified Bcc.Chain.Common as CC
import qualified Bcc.Chain.Genesis as Genesis
import qualified Bcc.Chain.Slotting as CC
import qualified Bcc.Chain.Update as CC

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.BlockchainTime
import           Shardagnostic.Consensus.Cole.Ledger.Orphans ()
import           Shardagnostic.Consensus.Config.SecurityParam
import           Shardagnostic.Consensus.Node.ProtocolInfo

{-------------------------------------------------------------------------------
  From @bcc-ledger-cole@ to @shardagnostic-consensus@
-------------------------------------------------------------------------------}

fromColeSlotNo :: CC.SlotNumber -> SlotNo
fromColeSlotNo = coerce

fromColeBlockNo :: CC.ChainDifficulty -> BlockNo
fromColeBlockNo = coerce

fromColeBlockCount :: CC.BlockCount -> SecurityParam
fromColeBlockCount (CC.BlockCount k) = SecurityParam k

fromColeEpochSlots :: CC.EpochSlots -> EpochSize
fromColeEpochSlots (CC.EpochSlots n) = EpochSize n

fromColeSlotLength :: Natural -> SlotLength
fromColeSlotLength = slotLengthFromMillisec
                    . (fromIntegral :: Natural -> Integer)

{-------------------------------------------------------------------------------
  From @shardagnostic-consensus@ to @bcc-ledger-cole@
-------------------------------------------------------------------------------}

toColeSlotNo :: SlotNo -> CC.SlotNumber
toColeSlotNo = coerce

toColeBlockCount :: SecurityParam -> CC.BlockCount
toColeBlockCount (SecurityParam k) = CC.BlockCount k

toColeSlotLength :: SlotLength -> Natural
toColeSlotLength = (fromIntegral :: Integer -> Natural)
                  . slotLengthToMillisec

{-------------------------------------------------------------------------------
  Extract info from genesis
-------------------------------------------------------------------------------}

genesisSecurityParam :: Genesis.Config -> SecurityParam
genesisSecurityParam =
      fromColeBlockCount
    . Genesis.gdK
    . Genesis.configGenesisData

genesisNumCoreNodes :: Genesis.Config -> NumCoreNodes
genesisNumCoreNodes =
      NumCoreNodes
    . fromIntegral
    . Set.size
    . Genesis.unGenesisKeyHashes
    . Genesis.gdGenesisKeyHashes
    . Genesis.configGenesisData

genesisSlotLength :: Genesis.Config -> Natural
genesisSlotLength =
      CC.ppSlotDuration
    . Genesis.configProtocolParameters
