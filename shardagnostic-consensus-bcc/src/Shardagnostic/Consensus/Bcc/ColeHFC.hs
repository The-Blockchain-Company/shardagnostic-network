{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Shardagnostic.Consensus.Bcc.ColeHFC (ColeBlockHFC) where

import qualified Data.Map.Strict as Map
import           Data.SOP.Strict

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Config
import           Shardagnostic.Consensus.Node.NetworkProtocolVersion
import           Shardagnostic.Consensus.Storage.Serialisation

import           Shardagnostic.Consensus.HardFork.Combinator
import           Shardagnostic.Consensus.HardFork.Combinator.Degenerate
import           Shardagnostic.Consensus.HardFork.Combinator.Serialisation.Common

import           Shardagnostic.Consensus.Cole.Ledger
import           Shardagnostic.Consensus.Cole.Node ()

import           Shardagnostic.Consensus.Bcc.CanHardFork
import           Shardagnostic.Consensus.Bcc.Node ()

{-------------------------------------------------------------------------------
  Synonym for convenience
-------------------------------------------------------------------------------}

-- | Cole as the single era in the hard fork combinator
type ColeBlockHFC = HardForkBlock '[ColeBlock]

{-------------------------------------------------------------------------------
  NoHardForks instance
-------------------------------------------------------------------------------}

instance NoHardForks ColeBlock where
  getEraParams cfg =
      coleEraParamsNeverHardForks (coleGenesisConfig (configBlock cfg))
  toPartialConsensusConfig _ = id
  toPartialLedgerConfig _ cfg = ColePartialLedgerConfig {
        coleLedgerConfig    = cfg
      , coleTriggerHardFork = TriggerHardForkNever
      }

{-------------------------------------------------------------------------------
  SupportedNetworkProtocolVersion instance
-------------------------------------------------------------------------------}

-- | Forward to the ColeBlock instance. Only supports
-- 'HardForkNodeToNodeDisabled', which is compatible with nodes running with
-- 'ColeBlock'.
instance SupportedNetworkProtocolVersion ColeBlockHFC where
  supportedNodeToNodeVersions _ =
      Map.map HardForkNodeToNodeDisabled $
      supportedNodeToNodeVersions (Proxy @ColeBlock)

  supportedNodeToClientVersions _ =
      Map.map HardForkNodeToClientDisabled $
      supportedNodeToClientVersions (Proxy @ColeBlock)

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

{-------------------------------------------------------------------------------
  SerialiseHFC instance
-------------------------------------------------------------------------------}

-- | Forward to the ColeBlock instance, this means we don't add an era
-- wrapper around blocks on disk. This makes sure we're compatible with the
-- existing Cole blocks.
instance SerialiseHFC '[ColeBlock] where
  encodeDiskHfcBlock (DegenCodecConfig ccfg) (DegenBlock b) =
      encodeDisk ccfg b
  decodeDiskHfcBlock (DegenCodecConfig ccfg) =
      fmap DegenBlock <$> decodeDisk ccfg
  reconstructHfcPrefixLen _ =
      reconstructPrefixLen (Proxy @(Header ColeBlock))
  reconstructHfcNestedCtxt _ prefix blockSize =
      mapSomeNestedCtxt NCZ $
        reconstructNestedCtxt (Proxy @(Header ColeBlock)) prefix blockSize
  getHfcBinaryBlockInfo (DegenBlock b) =
      getBinaryBlockInfo b
