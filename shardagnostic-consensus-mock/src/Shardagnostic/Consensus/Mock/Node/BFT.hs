module Shardagnostic.Consensus.Mock.Node.BFT (
    MockBftBlock
  , protocolInfoBft
  ) where

import qualified Data.Map.Strict as Map

import           Bcc.Crypto.DSIGN

import           Shardagnostic.Consensus.Config
import qualified Shardagnostic.Consensus.HardFork.History as HardFork
import           Shardagnostic.Consensus.HeaderValidation
import           Shardagnostic.Consensus.Ledger.Extended
import           Shardagnostic.Consensus.Mock.Ledger
import           Shardagnostic.Consensus.Mock.Node
import           Shardagnostic.Consensus.Node.ProtocolInfo
import           Shardagnostic.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Shardagnostic.Consensus.Protocol.BFT

type MockBftBlock = SimpleBftBlock SimpleMockCrypto BftMockCrypto

protocolInfoBft :: Monad m
                => NumCoreNodes
                -> CoreNodeId
                -> SecurityParam
                -> HardFork.EraParams
                -> ProtocolInfo m MockBftBlock
protocolInfoBft numCoreNodes nid securityParam eraParams =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            topLevelConfigProtocol = BftConfig {
                bftParams   = BftParams {
                                  bftNumNodes      = numCoreNodes
                                , bftSecurityParam = securityParam
                                }
              , bftSignKey  = signKey nid
              , bftVerKeys  = Map.fromList [
                    (CoreId n, verKey n)
                  | n <- enumCoreNodes numCoreNodes
                  ]
              }
          , topLevelConfigLedger  = SimpleLedgerConfig () eraParams
          , topLevelConfigBlock   = SimpleBlockConfig
          , topLevelConfigCodec   = SimpleCodecConfig
          , topLevelConfigStorage = SimpleStorageConfig securityParam
          }
      , pInfoInitLedger = ExtLedgerState (genesisSimpleLedgerState addrDist)
                                         (genesisHeaderState ())
      , pInfoBlockForging = return [simpleBlockForging nid forgeBftExt]
      }
  where
    signKey :: CoreNodeId -> SignKeyDSIGN MockDSIGN
    signKey (CoreNodeId n) = SignKeyMockDSIGN n

    verKey :: CoreNodeId -> VerKeyDSIGN MockDSIGN
    verKey (CoreNodeId n) = VerKeyMockDSIGN n

    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes
