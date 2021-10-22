module Shardagnostic.Consensus.ColeSpec.Ledger.Forge (forgeColeSpecBlock) where

import qualified Cole.Spec.Chain.STS.Block as Spec
import qualified Cole.Spec.Ledger.Core as Spec
import qualified Cole.Spec.Ledger.Update as Spec

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Ledger.Abstract

import           Shardagnostic.Consensus.ColeSpec.Ledger.Accessors
import           Shardagnostic.Consensus.ColeSpec.Ledger.Block
import           Shardagnostic.Consensus.ColeSpec.Ledger.Conversions
import qualified Shardagnostic.Consensus.ColeSpec.Ledger.GenTx as GenTx
import           Shardagnostic.Consensus.ColeSpec.Ledger.Ledger
import           Shardagnostic.Consensus.ColeSpec.Ledger.Mempool
import           Shardagnostic.Consensus.ColeSpec.Ledger.Orphans ()

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

forgeColeSpecBlock :: BlockNo
                    -> SlotNo
                    -> Ticked (LedgerState ColeSpecBlock)
                    -> [Validated (GenTx ColeSpecBlock)]
                    -> Spec.VKey
                    -> ColeSpecBlock
forgeColeSpecBlock curBlockNo curSlotNo (TickedColeSpecLedgerState _ st) txs vkey =
    ColeSpecBlock {
        coleSpecBlock     = block
      , coleSpecBlockNo   = curBlockNo
      , coleSpecBlockHash = Spec.bhHash $ Spec._bHeader block
      }
  where
    (ds, ts, us, vs) =
        GenTx.partition
          (map (unColeSpecGenTx . forgetValidatedColeSpecGenTx) txs)

    -- TODO: Don't take protocol version from ledger state
    -- <https://github.com/The-Blockchain-Company/shardagnostic-network/issues/1495>
    block :: Spec.Block
    block = Spec.mkBlock
              (getChainStateHash st)
              (toColeSpecSlotNo curSlotNo)
              vkey
              (Spec.protocolVersion $ getChainStateUPIState st)
              ds
              (case us of
                 []  -> Nothing
                 [u] -> Just u
                 _   -> error "forgeColeSpecBlock: multiple update proposals")
              vs
              ts
