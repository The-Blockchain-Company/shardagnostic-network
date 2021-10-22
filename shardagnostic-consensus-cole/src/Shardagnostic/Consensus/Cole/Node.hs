{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Shardagnostic.Consensus.Cole.Node (
    PBftSignatureThreshold (..)
  , ProtocolParamsCole (..)
  , coleBlockForging
  , defaultPBftSignatureThreshold
  , mkColeConfig
  , protocolClientInfoCole
  , protocolInfoCole
    -- * Secrets
  , ColeLeaderCredentials (..)
  , ColeLeaderCredentialsError
  , mkColeLeaderCredentials
  , mkPBftCanBeLeader
  ) where

import           Control.Monad.Except
import           Data.Coerce (coerce)
import           Data.Maybe
import           Data.Text (Text)
import           Data.Void (Void)

import qualified Bcc.Chain.Delegation as Delegation
import qualified Bcc.Chain.Genesis as Genesis
import           Bcc.Chain.ProtocolConstants (kEpochSlots)
import           Bcc.Chain.Slotting (EpochSlots (..))
import qualified Bcc.Chain.Update as Update
import qualified Bcc.Crypto as Crypto

import           Shardagnostic.Network.Magic (NetworkMagic (..))

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.BlockchainTime (SystemStart (..))
import           Shardagnostic.Consensus.Config
import           Shardagnostic.Consensus.Config.SupportsNode
import           Shardagnostic.Consensus.HeaderValidation
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.Ledger.Extended
import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits
import           Shardagnostic.Consensus.Node.InitStorage
import           Shardagnostic.Consensus.Node.ProtocolInfo
import           Shardagnostic.Consensus.Node.Run
import           Shardagnostic.Consensus.NodeId (CoreNodeId)
import           Shardagnostic.Consensus.Protocol.Abstract
import           Shardagnostic.Consensus.Protocol.PBFT
import qualified Shardagnostic.Consensus.Protocol.PBFT.State as S
import           Shardagnostic.Consensus.Storage.ChainDB.Init (InitChainDB (..))
import           Shardagnostic.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import           Shardagnostic.Consensus.Util ((....:))

import           Shardagnostic.Consensus.Cole.Crypto.DSIGN
import           Shardagnostic.Consensus.Cole.Ledger
import           Shardagnostic.Consensus.Cole.Ledger.Conversions
import           Shardagnostic.Consensus.Cole.Ledger.Inspect ()
import           Shardagnostic.Consensus.Cole.Node.Serialisation ()
import           Shardagnostic.Consensus.Cole.Protocol

{-------------------------------------------------------------------------------
  Credentials
-------------------------------------------------------------------------------}

-- | Credentials needed to produce blocks in the Cole era.
data ColeLeaderCredentials = ColeLeaderCredentials {
      blcSignKey    :: Crypto.SigningKey
    , blcDlgCert    :: Delegation.Certificate
      -- | Only core nodes can produce blocks. The 'CoreNodeId' is used to
      -- determine the order (round-robin) in which core nodes produce blocks.
    , blcCoreNodeId :: CoreNodeId
      -- | Identifier for this set of credentials.
      --
      -- Useful when the node is running with multiple sets of credentials.
    , blcLabel      :: Text
    }
  deriving (Show)

-- | Make the 'ColeLeaderCredentials', with a couple sanity checks:
--
-- * That the block signing key and the delegation certificate match.
-- * That the delegation certificate does correspond to one of the genesis
--   keys from the genesis file.
--
mkColeLeaderCredentials ::
     Genesis.Config
  -> Crypto.SigningKey
  -> Delegation.Certificate
  -> Text
  -> Either ColeLeaderCredentialsError ColeLeaderCredentials
mkColeLeaderCredentials gc sk cert lbl = do
    guard (Delegation.delegateVK cert == Crypto.toVerification sk)
      ?! NodeSigningKeyDoesNotMatchDelegationCertificate

    let vkGenesis = Delegation.issuerVK cert
    nid <- genesisKeyCoreNodeId gc (VerKeyColeDSIGN vkGenesis)
             ?! DelegationCertificateNotFromGenesisKey

    return ColeLeaderCredentials {
      blcSignKey     = sk
    , blcDlgCert     = cert
    , blcCoreNodeId  = nid
    , blcLabel       = lbl
    }
  where
    (?!) :: Maybe a -> e -> Either e a
    Just x  ?! _ = Right x
    Nothing ?! e = Left  e

data ColeLeaderCredentialsError =
       NodeSigningKeyDoesNotMatchDelegationCertificate
     | DelegationCertificateNotFromGenesisKey
  deriving (Eq, Show)

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

type instance CannotForge ColeBlock = PBftCannotForge PBftColeCrypto

type instance ForgeStateInfo ColeBlock = ()

type instance ForgeStateUpdateError ColeBlock = Void

coleBlockForging
  :: Monad m
  => TxLimits.Overrides ColeBlock
  -> ColeLeaderCredentials
  -> BlockForging m ColeBlock
coleBlockForging maxTxCapacityOverrides creds = BlockForging {
      forgeLabel       = blcLabel creds
    , canBeLeader
    , updateForgeState = \_ _ _ -> return $ ForgeStateUpdated ()
    , checkCanForge    = \cfg slot tickedPBftState _isLeader () ->
                             pbftCheckCanForge
                               (configConsensus cfg)
                               canBeLeader
                               slot
                               tickedPBftState
    , forgeBlock       = \cfg -> return ....: forgeColeBlock cfg maxTxCapacityOverrides
    }
  where
    canBeLeader = mkPBftCanBeLeader creds

mkPBftCanBeLeader :: ColeLeaderCredentials -> CanBeLeader (PBft PBftColeCrypto)
mkPBftCanBeLeader (ColeLeaderCredentials sk cert nid _) = PBftCanBeLeader {
      pbftCanBeLeaderCoreNodeId = nid
    , pbftCanBeLeaderSignKey    = SignKeyColeDSIGN sk
    , pbftCanBeLeaderDlgCert    = cert
    }

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

-- | See chapter 4.1 of
--   https://hydra.tbco.io/job/Bcc/bcc-ledger-specs/coleChainSpec/latest/download-by-type/doc-pdf/blockchain-spec
defaultPBftSignatureThreshold :: PBftSignatureThreshold
defaultPBftSignatureThreshold = PBftSignatureThreshold 0.22

-- | Parameters needed to run Cole
data ProtocolParamsCole = ProtocolParamsCole {
      coleGenesis                :: Genesis.Config
    , colePbftSignatureThreshold :: Maybe PBftSignatureThreshold
    , coleProtocolVersion        :: Update.ProtocolVersion
    , coleSoftwareVersion        :: Update.SoftwareVersion
    , coleLeaderCredentials      :: Maybe ColeLeaderCredentials
    , coleMaxTxCapacityOverrides :: TxLimits.Overrides ColeBlock
    }

protocolInfoCole ::
     forall m. Monad m
  => ProtocolParamsCole
  -> ProtocolInfo m ColeBlock
protocolInfoCole ProtocolParamsCole {
                      coleGenesis                = genesisConfig
                    , colePbftSignatureThreshold = mSigThresh
                    , coleProtocolVersion        = pVer
                    , coleSoftwareVersion        = sVer
                    , coleLeaderCredentials      = mLeaderCreds
                    , coleMaxTxCapacityOverrides = maxTxCapacityOverrides
                    } =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            topLevelConfigProtocol = PBftConfig {
                pbftParams = colePBftParams compactedGenesisConfig mSigThresh
              }
          , topLevelConfigLedger  = compactedGenesisConfig
          , topLevelConfigBlock   = blockConfig
          , topLevelConfigCodec   = mkColeCodecConfig compactedGenesisConfig
          , topLevelConfigStorage = ColeStorageConfig blockConfig
          }
      , pInfoInitLedger = ExtLedgerState {
            -- Important: don't pass the compacted genesis config to
            -- 'initColeLedgerState', it needs the full one, including the AVVM
            -- balances.
            ledgerState = initColeLedgerState genesisConfig Nothing
          , headerState = genesisHeaderState S.empty
          }
      , pInfoBlockForging =
            return
          $ fmap (coleBlockForging maxTxCapacityOverrides)
          $ maybeToList mLeaderCreds
      }
  where
    compactedGenesisConfig = compactGenesisConfig genesisConfig

    blockConfig = mkColeConfig compactedGenesisConfig pVer sVer

protocolClientInfoCole :: EpochSlots -> ProtocolClientInfo ColeBlock
protocolClientInfoCole epochSlots =
    ProtocolClientInfo {
      pClientInfoCodecConfig = ColeCodecConfig {
          getColeEpochSlots = epochSlots
        }
    }

colePBftParams :: Genesis.Config -> Maybe PBftSignatureThreshold -> PBftParams
colePBftParams cfg threshold = PBftParams {
      pbftSecurityParam      = genesisSecurityParam cfg
    , pbftNumNodes           = genesisNumCoreNodes  cfg
    , pbftSignatureThreshold = fromMaybe defaultPBftSignatureThreshold threshold
    }

mkColeConfig :: Genesis.Config
              -> Update.ProtocolVersion
              -> Update.SoftwareVersion
              -> BlockConfig ColeBlock
mkColeConfig genesisConfig pVer sVer = ColeConfig {
      coleGenesisConfig   = genesisConfig
    , coleProtocolVersion = pVer
    , coleSoftwareVersion = sVer
    }

{-------------------------------------------------------------------------------
  ConfigSupportsNode instance
-------------------------------------------------------------------------------}

instance ConfigSupportsNode ColeBlock where
  getSystemStart =
      SystemStart
    . Genesis.gdStartTime
    . extractGenesisData

  getNetworkMagic =
      NetworkMagic
    . Crypto.unProtocolMagicId
    . Genesis.gdProtocolMagicId
    . extractGenesisData

extractGenesisData :: BlockConfig ColeBlock -> Genesis.GenesisData
extractGenesisData = Genesis.configGenesisData . coleGenesisConfig

{-------------------------------------------------------------------------------
  NodeInitStorage instance
-------------------------------------------------------------------------------}

instance NodeInitStorage ColeBlock where
  -- The epoch size is fixed and can be derived from @k@ by the ledger
  -- ('kEpochSlots').
  nodeImmutableDbChunkInfo =
        simpleChunkInfo
      . (coerce :: EpochSlots -> EpochSize)
      . kEpochSlots
      . Genesis.gdK
      . extractGenesisData
      . getColeBlockConfig

  -- If the current chain is empty, produce a genesis EBB and add it to the
  -- ChainDB. Only an EBB can have Genesis (= empty chain) as its predecessor.
  nodeInitChainDB cfg InitChainDB { getCurrentLedger, addBlock } = do
      tip <- ledgerTipPoint (Proxy @ColeBlock) <$> getCurrentLedger
      case tip of
        BlockPoint {} -> return ()
        GenesisPoint  -> addBlock genesisEBB
    where
      genesisEBB =
        forgeEBB (getColeBlockConfig cfg) (SlotNo 0) (BlockNo 0) GenesisHash

  nodeCheckIntegrity = verifyBlockIntegrity . getColeBlockConfig

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance BlockSupportsMetrics ColeBlock where
  isSelfIssued = isSelfIssuedConstUnknown

instance RunNode ColeBlock
