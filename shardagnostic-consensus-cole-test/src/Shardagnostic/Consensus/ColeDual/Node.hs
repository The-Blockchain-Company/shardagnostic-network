{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Shardagnostic.Consensus.ColeDual.Node (protocolInfoDualCole) where

import           Data.Either (fromRight)
import           Data.Map.Strict (Map)
import           Data.Maybe (fromMaybe)

import qualified Cole.Spec.Ledger.Core as Spec
import qualified Cole.Spec.Ledger.Delegation as Spec
import qualified Cole.Spec.Ledger.UTxO as Spec
import qualified Cole.Spec.Ledger.Update as Spec

import qualified Test.Bcc.Chain.Elaboration.Block as Spec.Test
import qualified Test.Bcc.Chain.Elaboration.Delegation as Spec.Test
import qualified Test.Bcc.Chain.Elaboration.Keys as Spec.Test
import qualified Test.Bcc.Chain.Elaboration.Update as Spec.Test
import qualified Test.Bcc.Chain.UTxO.Model as Spec.Test

import qualified Bcc.Chain.Block as Impl
import qualified Bcc.Chain.Genesis as Impl
import qualified Bcc.Chain.UTxO as Impl
import qualified Bcc.Chain.Update as Impl
import qualified Bcc.Chain.Update.Validation.Interface as Impl

import           Shardagnostic.Consensus.HeaderValidation

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Config
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.Ledger.Dual
import           Shardagnostic.Consensus.Ledger.Extended
import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits
import           Shardagnostic.Consensus.Node.InitStorage
import           Shardagnostic.Consensus.Node.ProtocolInfo
import           Shardagnostic.Consensus.Node.Run
import           Shardagnostic.Consensus.NodeId
import           Shardagnostic.Consensus.Protocol.PBFT
import qualified Shardagnostic.Consensus.Protocol.PBFT.State as S
import           Shardagnostic.Consensus.Storage.ChainDB.Init (InitChainDB (..))
import           Shardagnostic.Consensus.Util ((.....:), (.:))

import           Shardagnostic.Consensus.Cole.Ledger
import           Shardagnostic.Consensus.Cole.Node
import           Shardagnostic.Consensus.Cole.Protocol

import           Shardagnostic.Consensus.ColeSpec.Ledger
import qualified Shardagnostic.Consensus.ColeSpec.Ledger.Genesis as Genesis

import           Shardagnostic.Consensus.ColeDual.Ledger
import           Shardagnostic.Consensus.ColeDual.Node.Serialisation ()

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

dualColeBlockForging
  :: Monad m
  => ColeLeaderCredentials
  -> BlockForging m DualColeBlock
dualColeBlockForging creds = BlockForging {
      forgeLabel       = forgeLabel
    , canBeLeader      = canBeLeader
    , updateForgeState = \cfg ->
        fmap castForgeStateUpdateInfo .: updateForgeState (dualTopLevelConfigMain cfg)
    , checkCanForge    = checkCanForge . dualTopLevelConfigMain
    , forgeBlock       = return .....: forgeDualColeBlock
    }
  where
    BlockForging {..} =
      coleBlockForging
        (TxLimits.mkOverrides TxLimits.noOverridesMeasure)
        creds

{-------------------------------------------------------------------------------
  ProtocolInfo

  Partly modelled after 'applyTrace' in "Test.Bcc.Chain.Block.Model".
-------------------------------------------------------------------------------}

protocolInfoDualCole :: forall m. Monad m
                      => ColeSpecGenesis
                      -> PBftParams
                      -> [CoreNodeId] -- ^ Are we a core node?
                      -> ProtocolInfo m DualColeBlock
protocolInfoDualCole abstractGenesis@ColeSpecGenesis{..} params credss =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            topLevelConfigProtocol = PBftConfig {
                pbftParams = params
              }
          , topLevelConfigLedger = DualLedgerConfig {
                dualLedgerConfigMain = concreteGenesis
              , dualLedgerConfigAux  = abstractConfig
              }
          , topLevelConfigBlock = DualBlockConfig {
                dualBlockConfigMain = concreteConfig
              , dualBlockConfigAux  = ColeSpecBlockConfig
              }
          , topLevelConfigCodec = DualCodecConfig {
                dualCodecConfigMain = mkColeCodecConfig concreteGenesis
              , dualCodecConfigAux  = ColeSpecCodecConfig
              }
          , topLevelConfigStorage = DualStorageConfig {
                dualStorageConfigMain = ColeStorageConfig concreteConfig
              , dualStorageConfigAux  = ColeSpecStorageConfig
              }
          }
      , pInfoInitLedger = ExtLedgerState {
             ledgerState = DualLedgerState {
                 dualLedgerStateMain   = initConcreteState
               , dualLedgerStateAux    = initAbstractState
               , dualLedgerStateBridge = initBridge
               }
           , headerState = genesisHeaderState S.empty
           }
      , pInfoBlockForging =
           return $ dualColeBlockForging . coleLeaderCredentials <$> credss
      }
  where
    initUtxo :: Impl.UTxO
    txIdMap  :: Map Spec.TxId Impl.TxId
    (initUtxo, txIdMap) = Spec.Test.elaborateInitialUTxO coleSpecGenesisInitUtxo

    -- 'Spec.Test.abEnvToCfg' ignores the UTxO, because the Cole genesis
    -- data doesn't contain a UTxO, but only a 'UTxOConfiguration'.
    --
    -- It also ignores the slot length (the Cole spec does not talk about
    -- slot lengths at all) so we have to set this ourselves.
    concreteGenesis :: Impl.Config
    concreteGenesis = translated {
          Impl.configGenesisData = configGenesisData {
              Impl.gdProtocolParameters = protocolParameters {
                   Impl.ppSlotDuration = coleSpecGenesisSlotLength
                }
            }
        }
      where
        translated = Spec.Test.abEnvToCfg $ Genesis.toChainEnv abstractGenesis
        configGenesisData  = Impl.configGenesisData translated
        protocolParameters = Impl.gdProtocolParameters configGenesisData

    initAbstractState :: LedgerState ColeSpecBlock
    initConcreteState :: LedgerState ColeBlock

    initAbstractState = initColeSpecLedgerState abstractGenesis
    initConcreteState = initColeLedgerState     concreteGenesis (Just initUtxo)

    abstractConfig :: LedgerConfig ColeSpecBlock
    concreteConfig :: BlockConfig ColeBlock

    abstractConfig = abstractGenesis
    concreteConfig = mkColeConfig
                       concreteGenesis
                       protocolVersion
                       softwareVersion
      where
        -- TODO: Take (spec) protocol version and (spec) software version
        -- as arguments instead, and then translate /those/ to Impl types.
        -- <https://github.com/The-Blockchain-Company/shardagnostic-network/issues/1495>
        protocolVersion :: Impl.ProtocolVersion
        protocolVersion =
            Impl.adoptedProtocolVersion $
              Impl.cvsUpdateState (coleLedgerState initConcreteState)

        -- The spec has a TODO about this; we just copy what 'elaborate' does
        -- (Test.Bcc.Chain.Elaboration.Block)
        softwareVersion :: Impl.SoftwareVersion
        softwareVersion =
            Spec.Test.elaborateSoftwareVersion $
              Spec.SwVer (Spec.ApName "") (Spec.ApVer 0)

    initBridge :: DualColeBridge
    initBridge = initColeSpecBridge abstractGenesis txIdMap

    coleLeaderCredentials :: CoreNodeId -> ColeLeaderCredentials
    coleLeaderCredentials nid =
        fromRight (error "coleLeaderCredentials: failed to construct credentials") $
          mkColeLeaderCredentials
            concreteGenesis
            (Spec.Test.vKeyToSKey vkey)
            (Spec.Test.elaborateDCert
               (Impl.configProtocolMagicId concreteGenesis)
               abstractDCert)
            "coleLeaderCredentials"
      where
        -- PBFT constructs the core node ID by the implicit ordering of
        -- the hashes of the verification keys in the genesis config. Here
        -- we go the other way, looking up this hash, and then using our
        -- translation map to find the corresponding abstract key.
        --
        -- TODO: We should be able to use keys that are /not/ in genesis
        -- (so that we can start the node with new delegated keys that aren't
        -- present in the genesis config).
        -- <https://github.com/The-Blockchain-Company/shardagnostic-network/issues/1495>
        keyHash :: PBftVerKeyHash PBftColeCrypto
        keyHash = fromMaybe
                    (error $ "mkCredentials: invalid " ++ show nid)
                    (nodeIdToGenesisKey concreteGenesis nid)

        vkey :: Spec.VKey
        vkey = bridgeToSpecKey initBridge keyHash

        abstractDCert :: Spec.DCert
        abstractDCert = Spec.Test.rcDCert
                          vkey
                          coleSpecGenesisSecurityParam
                          (coleSpecLedgerState initAbstractState)

{-------------------------------------------------------------------------------
  NodeInitStorage instance
-------------------------------------------------------------------------------}

instance NodeInitStorage DualColeBlock where
  -- Just like Cole, we need to start with an EBB
  nodeInitChainDB cfg InitChainDB { getCurrentLedger, addBlock } = do
      tip <- ledgerTipPoint (Proxy @DualColeBlock) <$> getCurrentLedger
      case tip of
        BlockPoint {} -> return ()
        GenesisPoint  -> addBlock genesisEBB
    where
      genesisEBB :: DualColeBlock
      genesisEBB = DualBlock {
            dualBlockMain   = coleEBB
          , dualBlockAux    = Nothing
          , dualBlockBridge = mempty
          }

      coleEBB :: ColeBlock
      coleEBB = forgeEBB
                   (getColeBlockConfig (dualStorageConfigMain cfg))
                   (SlotNo 0)
                   (BlockNo 0)
                   GenesisHash

  -- Node config is a consensus concern, determined by the main block only
  nodeImmutableDbChunkInfo = nodeImmutableDbChunkInfo . dualStorageConfigMain

  -- We don't really care too much about data loss or malicious behaviour for
  -- the dual ledger tests, so integrity and match checks can just use the
  -- concrete implementation
  nodeCheckIntegrity cfg = nodeCheckIntegrity (dualStorageConfigMain cfg) . dualBlockMain

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance BlockSupportsMetrics DualColeBlock where
  isSelfIssued = isSelfIssuedConstUnknown

instance RunNode DualColeBlock
