{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}

module Shardagnostic.Consensus.Sophie.Ledger.Forge (forgeSophieBlock) where

import           Control.Exception
import           Control.Monad.Except
import           Data.List (foldl')
import qualified Data.Sequence.Strict as Seq

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Config
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.Ledger.SupportsMempool
import           Shardagnostic.Consensus.Util.Assert

import qualified Bcc.Ledger.Core as Core (Tx)
import qualified Bcc.Ledger.Era as SL (hashTxSeq, toTxSeq)
import qualified Bcc.Protocol.TOptimum.BHeader as SL
import qualified Sophie.Spec.Ledger.API as SL (extractTx)
import qualified Sophie.Spec.Ledger.BlockChain as SL (Block (..), bBodySize)

import           Shardagnostic.Consensus.Mempool.TxLimits (TxLimits)
import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits
import           Shardagnostic.Consensus.Sophie.Eras (EraCrypto)
import           Shardagnostic.Consensus.Sophie.Ledger.Block
import           Shardagnostic.Consensus.Sophie.Ledger.Config
import           Shardagnostic.Consensus.Sophie.Ledger.Integrity
import           Shardagnostic.Consensus.Sophie.Ledger.Mempool
import           Shardagnostic.Consensus.Sophie.Protocol
import           Shardagnostic.Consensus.Sophie.Protocol.HotKey (HotKey)

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

forgeSophieBlock ::
     forall m era. (SophieBasedEra era, TxLimits (SophieBlock era), Monad m)
  => HotKey (EraCrypto era) m
  -> TOptimumCanBeLeader (EraCrypto era)
  -> TopLevelConfig (SophieBlock era)
  -> TxLimits.Overrides (SophieBlock era)     -- ^ How to override max tx
                                               -- capacity defined by ledger
  -> BlockNo                                   -- ^ Current block number
  -> SlotNo                                    -- ^ Current slot number
  -> TickedLedgerState (SophieBlock era)      -- ^ Current ledger
  -> [Validated (GenTx (SophieBlock era))]    -- ^ Txs to add in the block
  -> TOptimumIsLeader (EraCrypto era)            -- ^ Leader proof
  -> m (SophieBlock era)
forgeSophieBlock hotKey canBeLeader cfg maxTxCapacityOverrides curNo curSlot tickedLedger txs isLeader = do
    toptimumFields <- forgeTOptimumFields hotKey canBeLeader isLeader mkBhBody
    let blk = mkSophieBlock $ SL.Block (mkHeader toptimumFields) body
    return $
      assert (verifyBlockIntegrity toptimumSlotsPerKESPeriod blk) $
      assertWithMsg bodySizeEstimate blk
  where
    TOptimumConfig { toptimumParams = TOptimumParams { toptimumSlotsPerKESPeriod } } =
      configConsensus cfg

    body =
        SL.toTxSeq @era
      . Seq.fromList
      . fmap extractTx
      $ takeLargestPrefixThatFits maxTxCapacityOverrides tickedLedger txs

    extractTx :: Validated (GenTx (SophieBlock era)) -> Core.Tx era
    extractTx (SophieValidatedTx _txid vtx) = SL.extractTx vtx

    mkHeader TOptimumFields { toptimumSignature, toptimumToSign } =
      SL.BHeader toptimumToSign toptimumSignature

    prevHash :: SL.PrevHash (EraCrypto era)
    prevHash =
        toSophiePrevHash @era
      . castHash
      . getTipHash
      $ tickedLedger

    bodySizeEstimate :: Either String ()
    bodySizeEstimate
      | actualBodySize > estimatedBodySize + fixedBlockBodyOverhead
      = throwError $
          "Actual block body size > Estimated block body size + fixedBlockBodyOverhead: "
            <> show actualBodySize
            <> " > "
            <> show estimatedBodySize
            <> " + "
            <> show (fixedBlockBodyOverhead :: Int)
      | otherwise
      = return ()

    estimatedBodySize, actualBodySize :: Int
    estimatedBodySize = fromIntegral $ foldl' (+) 0 $ map (txInBlockSize . txForgetValidated) txs
    actualBodySize    = SL.bBodySize body

    mkBhBody toSign = SL.BHBody {
          bheaderPrev    = prevHash
        , bheaderVk      = toptimumToSignIssuerVK
        , bheaderVrfVk   = toptimumToSignVrfVK
        , bheaderSlotNo  = curSlot
        , bheaderBlockNo = curNo
        , bheaderEta     = toptimumToSignEta
        , bheaderL       = toptimumToSignLeader
        , bsize          = fromIntegral actualBodySize
        , bhash          = SL.hashTxSeq @era body
        , bheaderOCert   = toptimumToSignOCert
        , bprotver       = sophieProtocolVersion $ configBlock cfg
        }
      where
        TOptimumToSign {
            toptimumToSignIssuerVK
          , toptimumToSignVrfVK
          , toptimumToSignEta
          , toptimumToSignLeader
          , toptimumToSignOCert
          } = toSign
