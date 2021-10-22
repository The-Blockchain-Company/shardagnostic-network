{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Shardagnostic.Consensus.Mock.Node.Optimum (
    MockOptimumBlock
  , protocolInfoOptimum
  ) where

import           Data.Bifunctor (second)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Numeric.Natural (Natural)

import           Bcc.Crypto.KES
import           Bcc.Crypto.VRF

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Config
import qualified Shardagnostic.Consensus.HardFork.History as HardFork
import           Shardagnostic.Consensus.HeaderValidation
import           Shardagnostic.Consensus.Ledger.Extended
import           Shardagnostic.Consensus.Ledger.SupportsMempool (txForgetValidated)
import           Shardagnostic.Consensus.Mock.Ledger
import           Shardagnostic.Consensus.Mock.Protocol.Optimum
import           Shardagnostic.Consensus.Node.ProtocolInfo
import           Shardagnostic.Consensus.NodeId (CoreNodeId (..))
import           Shardagnostic.Consensus.Util.IOLike

type MockOptimumBlock = SimpleOptimumBlock SimpleMockCrypto OptimumMockCrypto

protocolInfoOptimum :: IOLike m
                  => NumCoreNodes
                  -> CoreNodeId
                  -> OptimumParams
                  -> HardFork.EraParams
                  -> Natural
                  -> OptimumEvolvingStake
                  -> ProtocolInfo m MockOptimumBlock
protocolInfoOptimum numCoreNodes nid params eraParams eta0 evolvingStakeDist =
    ProtocolInfo {
        pInfoConfig = TopLevelConfig {
            topLevelConfigProtocol = OptimumConfig {
                optimumParams        = params
              , optimumSignKeyVRF    = signKeyVRF nid
              , optimumInitialEta    = eta0
              , optimumInitialStake  = genesisStakeDist addrDist
              , optimumEvolvingStake = evolvingStakeDist
              , optimumVerKeys       = verKeys
              }
          , topLevelConfigLedger  = SimpleLedgerConfig addrDist eraParams
          , topLevelConfigBlock   = SimpleBlockConfig
          , topLevelConfigCodec   = SimpleCodecConfig
          , topLevelConfigStorage = SimpleStorageConfig (optimumSecurityParam params)
          }
      , pInfoInitLedger = ExtLedgerState {
            ledgerState = genesisSimpleLedgerState addrDist
          , headerState = genesisHeaderState (OptimumChainDepState [])
          }
      , pInfoBlockForging = sequence [optimumBlockForging nid initHotKey]
      }
  where
    signKeyVRF :: CoreNodeId -> SignKeyVRF MockVRF
    signKeyVRF (CoreNodeId n) = SignKeyMockVRF n

    verKeyVRF :: CoreNodeId -> VerKeyVRF MockVRF
    verKeyVRF (CoreNodeId n) = VerKeyMockVRF n

    verKeyKES :: CoreNodeId -> VerKeyKES (MockKES t)
    verKeyKES (CoreNodeId n) = VerKeyMockKES n

    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes

    verKeys :: Map CoreNodeId (VerKeyKES (MockKES t), VerKeyVRF MockVRF)
    verKeys = Map.fromList
      [ (nid', (kesKey, vrfKey))
      | nid' <- enumCoreNodes numCoreNodes
      , let !kesKey = verKeyKES nid'
            !vrfKey = verKeyVRF nid'
      ]

    initHotKey :: HotKey OptimumMockCrypto
    initHotKey =
        HotKey
          0
          (SignKeyMockKES
            -- key ID
            (fst $ verKeys Map.! nid)
            -- KES initial slot
            0)

optimumBlockForging ::
     IOLike m
  => CoreNodeId
  -> HotKey OptimumMockCrypto
  -> m (BlockForging m MockOptimumBlock)
optimumBlockForging cid initHotKey = do
    varHotKey <- newMVar initHotKey
    return $ BlockForging {
        forgeLabel       = "optimumBlockForging"
      , canBeLeader      = cid
      , updateForgeState = \_ sno _ -> updateMVar varHotKey $
                                 second forgeStateUpdateInfoFromUpdateInfo
                               . evolveKey sno
      , checkCanForge    = \_ _ _ _ _ -> return ()
      , forgeBlock       = \cfg bno sno tickedLedgerSt txs isLeader -> do
                               hotKey <- readMVar varHotKey
                               return $
                                 forgeSimple
                                   (forgeOptimumExt hotKey)
                                   cfg
                                   bno sno
                                   tickedLedgerSt
                                   (map txForgetValidated txs)
                                   isLeader
      }
