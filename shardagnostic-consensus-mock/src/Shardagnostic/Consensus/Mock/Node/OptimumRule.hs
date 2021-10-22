-- | Test the Optimum chain selection rule but with explicit leader schedule
module Shardagnostic.Consensus.Mock.Node.OptimumRule (
    MockOptimumRuleBlock
  , protocolInfoOptimumRule
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Bcc.Crypto.KES
import           Bcc.Crypto.VRF

import           Shardagnostic.Consensus.Config
import qualified Shardagnostic.Consensus.HardFork.History as HardFork
import           Shardagnostic.Consensus.HeaderValidation
import           Shardagnostic.Consensus.Ledger.Extended
import           Shardagnostic.Consensus.Mock.Ledger
import           Shardagnostic.Consensus.Mock.Node
import           Shardagnostic.Consensus.Mock.Protocol.LeaderSchedule
import           Shardagnostic.Consensus.Mock.Protocol.Optimum
import           Shardagnostic.Consensus.Node.ProtocolInfo
import           Shardagnostic.Consensus.NodeId (CoreNodeId (..))

type MockOptimumRuleBlock = SimpleOptimumRuleBlock SimpleMockCrypto

protocolInfoOptimumRule :: Monad m
                      => NumCoreNodes
                      -> CoreNodeId
                      -> OptimumParams
                      -> HardFork.EraParams
                      -> LeaderSchedule
                      -> OptimumEvolvingStake
                      -> ProtocolInfo m MockOptimumRuleBlock
protocolInfoOptimumRule numCoreNodes
                      nid
                      params
                      eraParams
                      schedule
                      evolvingStake =
    ProtocolInfo {
      pInfoConfig = TopLevelConfig {
          topLevelConfigProtocol = WLSConfig {
              wlsConfigSchedule = schedule
            , wlsConfigP        = OptimumConfig
                { optimumParams        = params
                , optimumSignKeyVRF    = NeverUsedSignKeyVRF
                , optimumInitialEta    = 0
                , optimumInitialStake  = genesisStakeDist addrDist
                , optimumEvolvingStake = evolvingStake
                , optimumVerKeys       = verKeys
                }
            , wlsConfigNodeId   = nid
            }
        , topLevelConfigLedger  = SimpleLedgerConfig () eraParams
        , topLevelConfigBlock   = SimpleBlockConfig
        , topLevelConfigCodec   = SimpleCodecConfig
        , topLevelConfigStorage = SimpleStorageConfig (optimumSecurityParam params)
        }
    , pInfoInitLedger = ExtLedgerState
        { ledgerState = genesisSimpleLedgerState addrDist
        , headerState = genesisHeaderState ()
        }
    , pInfoBlockForging = return [simpleBlockForging () forgeOptimumRuleExt]
    }
  where
    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes

    verKeys :: Map CoreNodeId (VerKeyKES NeverKES, VerKeyVRF NeverVRF)
    verKeys = Map.fromList [ (nid', (NeverUsedVerKeyKES, NeverUsedVerKeyVRF))
                           | nid' <- enumCoreNodes numCoreNodes
                           ]
