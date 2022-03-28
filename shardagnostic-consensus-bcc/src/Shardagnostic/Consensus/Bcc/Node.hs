{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-orphans
                -Wno-incomplete-patterns
                -Wno-incomplete-uni-patterns
                -Wno-incomplete-record-updates
                -Wno-overlapping-patterns #-}
module Shardagnostic.Consensus.Bcc.Node (
    BccHardForkConstraints
  , MaxMajorProtVer (..)
  , ProtocolParamsEvie (..)
  , ProtocolParamsJen (..)
  , ProtocolTransitionParamsSophieBased (..)
  , TriggerHardFork (..)
  , protocolClientInfoBcc
  , protocolInfoBcc
    -- * SupportedNetworkProtocolVersion
  , pattern BccNodeToClientVersion1
  , pattern BccNodeToClientVersion2
  , pattern BccNodeToClientVersion3
  , pattern BccNodeToClientVersion4
  , pattern BccNodeToClientVersion5
  , pattern BccNodeToClientVersion6
  , pattern BccNodeToClientVersion7
  , pattern BccNodeToNodeVersion1
  , pattern BccNodeToNodeVersion2
  , pattern BccNodeToNodeVersion3
  , pattern BccNodeToNodeVersion4
  , pattern BccNodeToNodeVersion5
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Control.Exception (assert)
import qualified Data.ByteString.Short as Short
import           Data.Functor.These (These1 (..))
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict hiding (shape, shift)
import           Data.Word (Word16)

import           Bcc.Binary (DecoderError (..), enforceSize)
import           Bcc.Chain.Slotting (EpochSlots)
import           Bcc.Prelude (cborError)

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Config
import qualified Shardagnostic.Consensus.HardFork.History as History
import           Shardagnostic.Consensus.HeaderValidation
import           Shardagnostic.Consensus.Ledger.Extended
import           Shardagnostic.Consensus.Node.NetworkProtocolVersion
import           Shardagnostic.Consensus.Node.ProtocolInfo
import           Shardagnostic.Consensus.Node.Run
import           Shardagnostic.Consensus.Storage.Serialisation
import           Shardagnostic.Consensus.Util.Assert
import           Shardagnostic.Consensus.Util.Counting
import           Shardagnostic.Consensus.Util.IOLike
import           Shardagnostic.Consensus.Util.OptNP (OptNP (..))
import qualified Shardagnostic.Consensus.Util.OptNP as OptNP
import           Shardagnostic.Consensus.Util.SOP (Index (..))

import           Shardagnostic.Consensus.HardFork.Combinator
import           Shardagnostic.Consensus.HardFork.Combinator.Embed.Nary
import           Shardagnostic.Consensus.HardFork.Combinator.Serialisation

import           Shardagnostic.Consensus.Cole.Ledger (ColeBlock)
import qualified Shardagnostic.Consensus.Cole.Ledger as Cole
import qualified Shardagnostic.Consensus.Cole.Ledger.Conversions as Cole
import           Shardagnostic.Consensus.Cole.Ledger.NetworkProtocolVersion
import           Shardagnostic.Consensus.Cole.Node

import qualified Bcc.Ledger.Era as Core
import qualified Sophie.Spec.Ledger.API as SL

import           Shardagnostic.Consensus.Sophie.Ledger (SophieBlock)
import qualified Shardagnostic.Consensus.Sophie.Ledger as Sophie
import           Shardagnostic.Consensus.Sophie.Ledger.NetworkProtocolVersion
import           Shardagnostic.Consensus.Sophie.Node
import           Shardagnostic.Consensus.Sophie.Protocol (TOptimumParams (..))
import qualified Shardagnostic.Consensus.Sophie.Protocol as Sophie
import           Shardagnostic.Consensus.Sophie.SophieBased

import           Shardagnostic.Consensus.Bcc.Block
import           Shardagnostic.Consensus.Bcc.CanHardFork
import           Shardagnostic.Consensus.Bcc.SophieBased

{-------------------------------------------------------------------------------
  SerialiseHFC
-------------------------------------------------------------------------------}

instance SerialiseConstraintsHFC ColeBlock

-- | Important: we need to maintain binary compatibility with Cole blocks, as
-- they are already stored on disk.
--
-- We also want to be able to efficiently detect (without having to peek far
-- ahead) whether we're dealing with a Cole or Sophie block, so that we can
-- invoke the right decoder. We plan to have a few more hard forks after
-- Sophie (Charles, Brandon, Sjobs), so we want a future-proof envelope for
-- distinguishing the different block types, i.e., a byte indicating the era.
--
-- Cole does not provide such an envelope. However, a Cole block is a CBOR
-- 2-tuple with the first element being a tag ('Word': 0 = EBB; 1 = regular
-- block) and the second being the payload. We can easily extend this encoding
-- format with support for Sophie, Charles, etc.
--
-- We encode a 'BccBlock' as the same CBOR 2-tuple as a Cole block, but
-- we use the tags after 1 for the hard forks after Cole:
--
-- 0. Cole EBB
-- 1. Cole regular block
-- 2. Sophie block
-- 3. Evie block
-- 4. Jen block
-- 5. Charles block
-- 6. etc.
--
-- For more details, see:
-- <https://github.com/The-Blockchain-Company/shardagnostic-network/pull/1175#issuecomment-558147194>
instance BccHardForkConstraints c => SerialiseHFC (BccEras c) where
  encodeDiskHfcBlock (BccCodecConfig ccfgCole ccfgSophie ccfgEvie ccfgJen ccfgAurum) = \case
      -- We are backwards compatible with Cole and thus use the exact same
      -- encoding.
      BlockCole   blockCole   ->                encodeDisk ccfgCole blockCole
      -- For Sophie and later eras, we need to prepend the hard fork envelope.
      BlockSophie blockSophie -> prependTag 2 $ encodeDisk ccfgSophie blockSophie
      BlockEvie blockEvie -> prependTag 3 $ encodeDisk ccfgEvie blockEvie
      BlockJen    blockJen    -> prependTag 4 $ encodeDisk ccfgJen    blockJen
      BlockAurum  blockAurum  -> prependTag 5 $ encodeDisk ccfgAurum  blockAurum
  decodeDiskHfcBlock (BccCodecConfig ccfgCole ccfgSophie ccfgEvie ccfgJen ccfgAurum) = do
      enforceSize "BccBlock" 2
      CBOR.decodeWord >>= \case
        0 -> fmap BlockCole   <$> Cole.decodeColeBoundaryBlock epochSlots
        1 -> fmap BlockCole   <$> Cole.decodeColeRegularBlock  epochSlots
        -- We don't have to drop the first two bytes from the 'ByteString'
        -- passed to the decoder as slicing already takes care of this.
        2 -> fmap BlockSophie <$> decodeDisk ccfgSophie
        3 -> fmap BlockEvie <$> decodeDisk ccfgEvie
        4 -> fmap BlockJen    <$> decodeDisk ccfgJen
        5 -> fmap BlockAurum  <$> decodeDisk ccfgAurum
        t -> cborError $ DecoderErrorUnknownTag "BccBlock" (fromIntegral t)
    where
      epochSlots = Cole.getColeEpochSlots ccfgCole

  reconstructHfcPrefixLen _ = PrefixLen 2

  reconstructHfcNestedCtxt _ prefix blockSize =
      case Short.index prefix 1 of
        0 -> SomeSecond $ NestedCtxt (NCZ (Cole.CtxtColeBoundary blockSize))
        1 -> SomeSecond $ NestedCtxt (NCZ (Cole.CtxtColeRegular  blockSize))
        2 -> SomeSecond $ NestedCtxt (NCS (NCZ Sophie.CtxtSophie))
        3 -> SomeSecond $ NestedCtxt (NCS (NCS (NCZ Sophie.CtxtSophie)))
        4 -> SomeSecond $ NestedCtxt (NCS (NCS (NCS (NCZ Sophie.CtxtSophie))))
        5 -> SomeSecond $ NestedCtxt (NCS (NCS (NCS (NCS (NCZ Sophie.CtxtSophie)))))
        _ -> error $ "BccBlock: invalid prefix " <> show prefix

  getHfcBinaryBlockInfo = \case
      BlockCole   blockCole   ->
        getBinaryBlockInfo blockCole
      -- For Sophie and the later eras, we need to account for the two extra
      -- bytes of the envelope.
      BlockSophie blockSophie ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockSophie
      BlockEvie blockEvie ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockEvie
      BlockJen blockJen ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockJen
      BlockAurum blockAurum ->
        shiftHeaderOffset 2 $ getBinaryBlockInfo blockAurum
    where
      shiftHeaderOffset :: Word16 -> BinaryBlockInfo -> BinaryBlockInfo
      shiftHeaderOffset shift binfo = binfo {
            headerOffset = headerOffset binfo + shift
          }

  estimateHfcBlockSize = \case
      HeaderCole   headerCole   -> estimateBlockSize headerCole
      -- For Sophie and later eras, we add two extra bytes, see the
      -- 'SerialiseHFC' instance.
      HeaderSophie headerSophie -> estimateBlockSize headerSophie + 2
      HeaderEvie headerEvie -> estimateBlockSize headerEvie + 2
      HeaderJen    headerJen    -> estimateBlockSize headerJen    + 2
      HeaderAurum  headerAurum  -> estimateBlockSize headerAurum  + 2

-- | Prepend the given tag by creating a CBOR 2-tuple with the tag as the
-- first element and the given 'Encoding' as the second.
prependTag :: Word -> Encoding -> Encoding
prependTag tag payload = mconcat [
      CBOR.encodeListLen 2
    , CBOR.encodeWord tag
    , payload
    ]

{-------------------------------------------------------------------------------
  SupportedNetworkProtocolVersion instance
-------------------------------------------------------------------------------}

-- Note: we don't support all combinations, so we don't declare them as
-- COMPLETE

-- | We support only Cole V1 with the hard fork disabled, as no other
-- versions have been released before the hard fork
pattern BccNodeToNodeVersion1 :: BlockNodeToNodeVersion (BccBlock c)
pattern BccNodeToNodeVersion1 =
    HardForkNodeToNodeDisabled ColeNodeToNodeVersion1

-- | The hard fork enabled with the latest Cole version and the Sophie era
-- enabled.
pattern BccNodeToNodeVersion2 :: BlockNodeToNodeVersion (BccBlock c)
pattern BccNodeToNodeVersion2 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ColeNodeToNodeVersion2
      :* EraNodeToNodeEnabled SophieNodeToNodeVersion1
      :* EraNodeToNodeDisabled
      :* EraNodeToNodeDisabled
      :* EraNodeToNodeDisabled
      :* Nil
      )

-- | The hard fork enabled with the latest Cole version, the Sophie and
-- Evie eras enabled.
pattern BccNodeToNodeVersion3 :: BlockNodeToNodeVersion (BccBlock c)
pattern BccNodeToNodeVersion3 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ColeNodeToNodeVersion2
      :* EraNodeToNodeEnabled SophieNodeToNodeVersion1
      :* EraNodeToNodeEnabled SophieNodeToNodeVersion1
      :* EraNodeToNodeDisabled
      :* EraNodeToNodeDisabled
      :* Nil
      )

-- | The hard fork enabled with the latest Cole version, the Sophie, Evie,
-- and Jen eras enabled.
pattern BccNodeToNodeVersion4 :: BlockNodeToNodeVersion (BccBlock c)
pattern BccNodeToNodeVersion4 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ColeNodeToNodeVersion2
      :* EraNodeToNodeEnabled SophieNodeToNodeVersion1
      :* EraNodeToNodeEnabled SophieNodeToNodeVersion1
      :* EraNodeToNodeEnabled SophieNodeToNodeVersion1
      :* EraNodeToNodeDisabled
      :* Nil
      )

-- | The hard fork enabled with the latest Cole version, the Sophie, Evie,
-- Jen and Aurum eras enabled.
pattern BccNodeToNodeVersion5 :: BlockNodeToNodeVersion (BccBlock c)
pattern BccNodeToNodeVersion5 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled ColeNodeToNodeVersion2
      :* EraNodeToNodeEnabled SophieNodeToNodeVersion1
      :* EraNodeToNodeEnabled SophieNodeToNodeVersion1
      :* EraNodeToNodeEnabled SophieNodeToNodeVersion1
      :* EraNodeToNodeEnabled SophieNodeToNodeVersion1
      :* Nil
      )

-- | We support the sole Cole version with the hard fork disabled.
pattern BccNodeToClientVersion1 :: BlockNodeToClientVersion (BccBlock c)
pattern BccNodeToClientVersion1 =
    HardForkNodeToClientDisabled ColeNodeToClientVersion1

-- | The hard fork enabled and the Sophie era enabled.
pattern BccNodeToClientVersion2 :: BlockNodeToClientVersion (BccBlock c)
pattern BccNodeToClientVersion2 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion1
      (  EraNodeToClientEnabled ColeNodeToClientVersion1
      :* EraNodeToClientEnabled SophieNodeToClientVersion1
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled and the Sophie era enabled, but using
-- 'SophieNodeToClientVersion2' and 'HardForkSpecificNodeToClientVersion2'.
pattern BccNodeToClientVersion3 :: BlockNodeToClientVersion (BccBlock c)
pattern BccNodeToClientVersion3 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ColeNodeToClientVersion1
      :* EraNodeToClientEnabled SophieNodeToClientVersion2
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Sophie and Evie eras enabled.
--
-- We don't bother with 'SophieNodeToClientVersion1' and
-- 'HardForkSpecificNodeToClientVersion1'.
pattern BccNodeToClientVersion4 :: BlockNodeToClientVersion (BccBlock c)
pattern BccNodeToClientVersion4 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ColeNodeToClientVersion1
      :* EraNodeToClientEnabled SophieNodeToClientVersion2
      :* EraNodeToClientEnabled SophieNodeToClientVersion2
      :* EraNodeToClientDisabled
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Sophie, Evie, and Jen eras enabled.
--
-- We don't bother with 'SophieNodeToClientVersion1'.
pattern BccNodeToClientVersion5 :: BlockNodeToClientVersion (BccBlock c)
pattern BccNodeToClientVersion5 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ColeNodeToClientVersion1
      :* EraNodeToClientEnabled SophieNodeToClientVersion2
      :* EraNodeToClientEnabled SophieNodeToClientVersion2
      :* EraNodeToClientEnabled SophieNodeToClientVersion2
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Sophie, Evie, and Jen eras enabled, but
-- using 'SophieNodeToClientVersion3' for the Sophie-based eras , which
-- enables new queries.
pattern BccNodeToClientVersion6 :: BlockNodeToClientVersion (BccBlock c)
pattern BccNodeToClientVersion6 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ColeNodeToClientVersion1
      :* EraNodeToClientEnabled SophieNodeToClientVersion3
      :* EraNodeToClientEnabled SophieNodeToClientVersion3
      :* EraNodeToClientEnabled SophieNodeToClientVersion3
      :* EraNodeToClientDisabled
      :* Nil
      )

-- | The hard fork enabled, and the Sophie, Evie, Jen and Aurum eras enabled
pattern BccNodeToClientVersion7 :: BlockNodeToClientVersion (BccBlock c)
pattern BccNodeToClientVersion7 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled ColeNodeToClientVersion1
      :* EraNodeToClientEnabled SophieNodeToClientVersion4
      :* EraNodeToClientEnabled SophieNodeToClientVersion4
      :* EraNodeToClientEnabled SophieNodeToClientVersion4
      :* EraNodeToClientEnabled SophieNodeToClientVersion4
      :* Nil
      )

instance BccHardForkConstraints c
      => SupportedNetworkProtocolVersion (BccBlock c) where
  supportedNodeToNodeVersions _ = Map.fromList $
      [ (NodeToNodeV_1, BccNodeToNodeVersion1)
      , (NodeToNodeV_2, BccNodeToNodeVersion2)
      , (NodeToNodeV_3, BccNodeToNodeVersion2)
      , (NodeToNodeV_4, BccNodeToNodeVersion3)
      , (NodeToNodeV_5, BccNodeToNodeVersion4)
      , (NodeToNodeV_6, BccNodeToNodeVersion4)
      , (NodeToNodeV_7, BccNodeToNodeVersion5)
      ]

  supportedNodeToClientVersions _ = Map.fromList $
      [ (NodeToClientV_1, BccNodeToClientVersion1)
      , (NodeToClientV_2, BccNodeToClientVersion1)
      , (NodeToClientV_3, BccNodeToClientVersion2)
      , (NodeToClientV_4, BccNodeToClientVersion3)
      , (NodeToClientV_5, BccNodeToClientVersion4)
      , (NodeToClientV_6, BccNodeToClientVersion5)
      , (NodeToClientV_7, BccNodeToClientVersion6)
      , (NodeToClientV_8, BccNodeToClientVersion6)
      , (NodeToClientV_9, BccNodeToClientVersion7)
      ]

  latestReleasedNodeVersion _prx = (Just NodeToNodeV_7, Just NodeToClientV_9)

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

-- | Parameters needed to transition to a Sophie era.
data ProtocolTransitionParamsSophieBased era = ProtocolTransitionParamsSophieBased {
      transitionTranslationContext :: Core.TranslationContext era
    , transitionTrigger            :: TriggerHardFork
    }

-- | Create a 'ProtocolInfo' for 'BccBlock'
--
-- NOTE: the initial staking and funds in the 'SophieGenesis' are ignored,
-- /unless/ configured to skip the Cole era and hard fork to Sophie or a later
-- era from the start using @TriggerHardForkAtEpoch 0@ for testing purposes.
--
-- PRECONDITION: only a single set of Sophie credentials is allowed when used
-- for mainnet (check against @'SL.gNetworkId' 'sophieBasedGenesis'@).
protocolInfoBcc ::
     forall c m. (IOLike m, BccHardForkConstraints c)
  => ProtocolParamsCole
  -> ProtocolParamsSophieBased (SophieEra c)
  -> ProtocolParamsSophie c
  -> ProtocolParamsEvie c
  -> ProtocolParamsJen    c
  -> ProtocolParamsAurum  c
  -> ProtocolTransitionParamsSophieBased (SophieEra c)
  -> ProtocolTransitionParamsSophieBased (EvieEra c)
  -> ProtocolTransitionParamsSophieBased (JenEra c)
  -> ProtocolTransitionParamsSophieBased (AurumEra c)
  -> ProtocolInfo m (BccBlock c)
protocolInfoBcc protocolParamsCole@ProtocolParamsCole {
                        coleGenesis                = genesisCole
                      , coleLeaderCredentials      = mCredsCole
                      , coleMaxTxCapacityOverrides = maxTxCapacityOverridesCole
                      }
                    ProtocolParamsSophieBased {
                        sophieBasedGenesis           = genesisSophie
                      , sophieBasedInitialNonce      = initialNonceSophie
                      , sophieBasedLeaderCredentials = credssSophieBased
                      }
                    ProtocolParamsSophie {
                        sophieProtVer                = protVerSophie
                      , sophieMaxTxCapacityOverrides = maxTxCapacityOverridesSophie
                      }
                    ProtocolParamsEvie {
                        evieProtVer                = protVerEvie
                      , evieMaxTxCapacityOverrides = maxTxCapacityOverridesEvie
                      }
                    ProtocolParamsJen {
                        jenProtVer                = protVerJen
                      , jenMaxTxCapacityOverrides = maxTxCapacityOverridesJen
                      }
                    ProtocolParamsAurum {
                        aurumProtVer                = protVerAurum
                      , aurumMaxTxCapacityOverrides = maxTxCapacityOverridesAurum
                      }
                    ProtocolTransitionParamsSophieBased {
                        transitionTranslationContext = ()
                      , transitionTrigger            = triggerHardForkSophie
                      }
                    ProtocolTransitionParamsSophieBased {
                        transitionTranslationContext = ()
                      , transitionTrigger            = triggerHardForkEvie
                      }
                    ProtocolTransitionParamsSophieBased {
                        transitionTranslationContext = ()
                      , transitionTrigger            = triggerHardForkJen
                      }
                    ProtocolTransitionParamsSophieBased {
                        transitionTranslationContext = transCtxtAurum
                      , transitionTrigger            = triggerHardForkAurum
                      }
  | SL.Mainnet <- SL.sgNetworkId genesisSophie
  , length credssSophieBased > 1
  = error "Multiple Sophie-based credentials not allowed for mainnet"
  | otherwise
  = assertWithMsg (validateGenesis genesisSophie) $
    ProtocolInfo {
        pInfoConfig       = cfg
      , pInfoInitLedger   = initExtLedgerStateBcc
      , pInfoBlockForging = blockForging
      }
  where
    -- The major protocol version of the last era is the maximum major protocol
    -- version we support.
    maxMajorProtVer :: MaxMajorProtVer
    maxMajorProtVer = MaxMajorProtVer (pvMajor protVerAurum)

    -- Cole

    ProtocolInfo {
        pInfoConfig = topLevelConfigCole@TopLevelConfig {
            topLevelConfigProtocol = consensusConfigCole
          , topLevelConfigLedger   = ledgerConfigCole
          , topLevelConfigBlock    = blockConfigCole
          }
      , pInfoInitLedger = initExtLedgerStateCole
      } = protocolInfoCole @m protocolParamsCole

    partialConsensusConfigCole :: PartialConsensusConfig (BlockProtocol ColeBlock)
    partialConsensusConfigCole = consensusConfigCole

    partialLedgerConfigCole :: PartialLedgerConfig ColeBlock
    partialLedgerConfigCole = ColePartialLedgerConfig {
          coleLedgerConfig    = ledgerConfigCole
        , coleTriggerHardFork = triggerHardForkSophie
        }

    kCole :: SecurityParam
    kCole = Cole.genesisSecurityParam genesisCole

    -- Sophie

    toptimumParams :: TOptimumParams
    toptimumParams@TOptimumParams { toptimumSlotsPerKESPeriod } =
        Sophie.mkTOptimumParams
          maxMajorProtVer
          initialNonceSophie
          genesisSophie

    blockConfigSophie :: BlockConfig (SophieBlock (SophieEra c))
    blockConfigSophie =
        Sophie.mkSophieBlockConfig
          protVerSophie
          genesisSophie
          (toptimumBlockIssuerVKey <$> credssSophieBased)

    partialConsensusConfigSophie ::
         PartialConsensusConfig (BlockProtocol (SophieBlock (SophieEra c)))
    partialConsensusConfigSophie = toptimumParams

    partialLedgerConfigSophie :: PartialLedgerConfig (SophieBlock (SophieEra c))
    partialLedgerConfigSophie =
        mkPartialLedgerConfigSophie
          genesisSophie
          ()  -- trivial translation context
          maxMajorProtVer
          triggerHardForkEvie

    kSophie :: SecurityParam
    kSophie = SecurityParam $ sgSecurityParam genesisSophie

    -- Evie

    genesisEvie :: SophieGenesis (EvieEra c)
    genesisEvie = Core.translateEra' () genesisSophie

    blockConfigEvie :: BlockConfig (SophieBlock (EvieEra c))
    blockConfigEvie =
        Sophie.mkSophieBlockConfig
          protVerEvie
          genesisEvie
          (toptimumBlockIssuerVKey <$> credssSophieBased)

    partialConsensusConfigEvie ::
         PartialConsensusConfig (BlockProtocol (SophieBlock (EvieEra c)))
    partialConsensusConfigEvie = toptimumParams

    partialLedgerConfigEvie :: PartialLedgerConfig (SophieBlock (EvieEra c))
    partialLedgerConfigEvie =
        mkPartialLedgerConfigSophie
          genesisEvie
          ()  -- trivial translation context
          maxMajorProtVer
          triggerHardForkJen

    -- Jen

    genesisJen :: SophieGenesis (JenEra c)
    genesisJen = Core.translateEra' () genesisEvie

    blockConfigJen :: BlockConfig (SophieBlock (JenEra c))
    blockConfigJen =
        Sophie.mkSophieBlockConfig
          protVerJen
          genesisJen
          (toptimumBlockIssuerVKey <$> credssSophieBased)

    partialConsensusConfigJen ::
         PartialConsensusConfig (BlockProtocol (SophieBlock (JenEra c)))
    partialConsensusConfigJen = toptimumParams

    partialLedgerConfigJen :: PartialLedgerConfig (SophieBlock (JenEra c))
    partialLedgerConfigJen =
        mkPartialLedgerConfigSophie
          genesisJen
          ()  -- trivial translation context
          maxMajorProtVer
          triggerHardForkAurum

    -- Aurum

    genesisAurum :: SophieGenesis (AurumEra c)
    genesisAurum = Core.translateEra' transCtxtAurum genesisJen

    blockConfigAurum :: BlockConfig (SophieBlock (AurumEra c))
    blockConfigAurum =
        Sophie.mkSophieBlockConfig
          protVerAurum
          genesisAurum
          (toptimumBlockIssuerVKey <$> credssSophieBased)

    partialConsensusConfigAurum ::
         PartialConsensusConfig (BlockProtocol (SophieBlock (AurumEra c)))
    partialConsensusConfigAurum = toptimumParams

    partialLedgerConfigAurum :: PartialLedgerConfig (SophieBlock (AurumEra c))
    partialLedgerConfigAurum =
        mkPartialLedgerConfigSophie
          genesisAurum
          transCtxtAurum
          maxMajorProtVer
          TriggerHardForkNever

    -- Bcc

    k :: SecurityParam
    k = assert (kCole == kSophie) kCole

    shape :: History.Shape (BccEras c)
    shape = History.Shape $ Exactly $
           K (Cole.coleEraParams     genesisCole)
        :* K (Sophie.sophieEraParams genesisSophie)
        :* K (Sophie.sophieEraParams genesisEvie)
        :* K (Sophie.sophieEraParams genesisJen)
        :* K (Sophie.sophieEraParams genesisAurum)
        :* Nil

    cfg :: TopLevelConfig (BccBlock c)
    cfg = TopLevelConfig {
        topLevelConfigProtocol = HardForkConsensusConfig {
            hardForkConsensusConfigK      = k
          , hardForkConsensusConfigShape  = shape
          , hardForkConsensusConfigPerEra = PerEraConsensusConfig
              (  WrapPartialConsensusConfig partialConsensusConfigCole
              :* WrapPartialConsensusConfig partialConsensusConfigSophie
              :* WrapPartialConsensusConfig partialConsensusConfigEvie
              :* WrapPartialConsensusConfig partialConsensusConfigJen
              :* WrapPartialConsensusConfig partialConsensusConfigAurum
              :* Nil
              )
          }
      , topLevelConfigLedger = HardForkLedgerConfig {
            hardForkLedgerConfigShape  = shape
          , hardForkLedgerConfigPerEra = PerEraLedgerConfig
              (  WrapPartialLedgerConfig partialLedgerConfigCole
              :* WrapPartialLedgerConfig partialLedgerConfigSophie
              :* WrapPartialLedgerConfig partialLedgerConfigEvie
              :* WrapPartialLedgerConfig partialLedgerConfigJen
              :* WrapPartialLedgerConfig partialLedgerConfigAurum
              :* Nil
              )
          }
      , topLevelConfigBlock =
          BccBlockConfig
            blockConfigCole
            blockConfigSophie
            blockConfigEvie
            blockConfigJen
            blockConfigAurum
      , topLevelConfigCodec =
          BccCodecConfig
            (configCodec topLevelConfigCole)
            Sophie.SophieCodecConfig
            Sophie.SophieCodecConfig
            Sophie.SophieCodecConfig
            Sophie.SophieCodecConfig
      , topLevelConfigStorage =
          BccStorageConfig
            (configStorage topLevelConfigCole)
            (Sophie.SophieStorageConfig toptimumSlotsPerKESPeriod k)
            (Sophie.SophieStorageConfig toptimumSlotsPerKESPeriod k)
            (Sophie.SophieStorageConfig toptimumSlotsPerKESPeriod k)
            (Sophie.SophieStorageConfig toptimumSlotsPerKESPeriod k)
      }

    -- When the initial ledger state is not in the Cole era, register the
    -- initial staking and initial funds (if provided in the genesis config) in
    -- the ledger state.
    initExtLedgerStateBcc :: ExtLedgerState (BccBlock c)
    initExtLedgerStateBcc = ExtLedgerState {
          headerState = initHeaderState
        , ledgerState = overSophieBasedLedgerState register initLedgerState
        }
      where
        initHeaderState :: HeaderState (BccBlock c)
        initLedgerState :: LedgerState (BccBlock c)
        ExtLedgerState initLedgerState initHeaderState =
          injectInitialExtLedgerState cfg initExtLedgerStateCole

        register ::
             (EraCrypto era ~ c, SophieBasedEra era)
          => LedgerState (SophieBlock era)
          -> LedgerState (SophieBlock era)
        register st = st {
              Sophie.sophieLedgerState =
                -- We must first register the initial funds, because the stake
                -- information depends on it.
                  registerGenesisStaking
                    (SL.sgStaking genesisSophie)
                . registerInitialFunds
                    (SL.sgInitialFunds genesisSophie)
                $ Sophie.sophieLedgerState st
            }

    -- | For each element in the list, a block forging thread will be started.
    --
    -- When no credentials are passed, there will be no threads.
    --
    -- Typically, there will only be a single set of credentials for Sophie.
    --
    -- In case there are multiple credentials for Sophie, which is only done
    -- for testing/benchmarking purposes, we'll have a separate thread for each
    -- of them.
    --
    -- If Cole credentials are passed, we merge them with the Sophie
    -- credentials if possible, so that we only have a single thread running in
    -- the case we have Cole credentials and a single set of Sophie
    -- credentials. If there are multiple Sophie credentials, we merge the
    -- Cole credentials with the first Sophie one but still have separate
    -- threads for the remaining Sophie ones.
    blockForging :: m [BlockForging m (BccBlock c)]
    blockForging = do
        sophieBased <- blockForgingSophieBased
        let blockForgings :: [OptNP 'False (BlockForging m) (BccEras c)]
            blockForgings = case (mBlockForgingCole, sophieBased) of
              (Nothing,    sophies)         -> sophies
              (Just cole, [])               -> [cole]
              (Just cole, sophie:sophies) ->
                  OptNP.zipWith merge cole sophie : sophies
                where
                  -- When merging Cole with Sophie-based eras, we should never
                  -- merge two from the same era.
                  merge (These1 _ _) = error "forgings of the same era"
                  merge (This1 x)    = x
                  merge (That1 y)    = y

        return $ hardForkBlockForging "Bcc" <$> blockForgings

    mBlockForgingCole :: Maybe (OptNP 'False (BlockForging m) (BccEras c))
    mBlockForgingCole = do
        creds <- mCredsCole
        return $ coleBlockForging maxTxCapacityOverridesCole creds `OptNP.at` IZ

    blockForgingSophieBased :: m [OptNP 'False (BlockForging m) (BccEras c)]
    blockForgingSophieBased = do
        sophieBased <-
          traverse
            (\creds -> sophieSharedBlockForging
               (Proxy @(SophieBasedEras c))
               toptimumParams
               creds
               maxTxCapacityOverridess
            )
            credssSophieBased

        return $ reassoc <$> sophieBased
      where
        reassoc ::
             NP (BlockForging m :.: SophieBlock) (SophieBasedEras c)
          -> OptNP 'False (BlockForging m) (BccEras c)
        reassoc = OptSkip . injectSophieOptNP unComp . OptNP.fromNonEmptyNP

        maxTxCapacityOverridess =
          Comp maxTxCapacityOverridesSophie :*
          Comp maxTxCapacityOverridesEvie :*
          Comp maxTxCapacityOverridesJen    :*
          Comp maxTxCapacityOverridesAurum  :*
          Nil

protocolClientInfoBcc
  :: forall c.
     -- Cole
     EpochSlots
  -> ProtocolClientInfo (BccBlock c)
protocolClientInfoBcc epochSlots = ProtocolClientInfo {
      pClientInfoCodecConfig =
        BccCodecConfig
          (pClientInfoCodecConfig (protocolClientInfoCole epochSlots))
          (pClientInfoCodecConfig protocolClientInfoSophie)
          (pClientInfoCodecConfig protocolClientInfoSophie)
          (pClientInfoCodecConfig protocolClientInfoSophie)
          (pClientInfoCodecConfig protocolClientInfoSophie)
    }

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

mkPartialLedgerConfigSophie ::
     SophieGenesis era
  -> Core.TranslationContext era
  -> MaxMajorProtVer
  -> TriggerHardFork
  -> PartialLedgerConfig (SophieBlock era)
mkPartialLedgerConfigSophie
  genesisSophie
  transCtxt
  maxMajorProtVer
  sophieTriggerHardFork =
    SophiePartialLedgerConfig {
          sophieLedgerConfig =
            Sophie.mkSophieLedgerConfig
              genesisSophie
              transCtxt
              -- 'completeLedgerConfig' will replace the 'History.dummyEpochInfo'
              -- in the partial ledger config with the correct one.
              History.dummyEpochInfo
              maxMajorProtVer
        , sophieTriggerHardFork = sophieTriggerHardFork
        }
