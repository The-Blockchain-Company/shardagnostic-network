{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.ThreadNet.SophieEvie (tests) where

import           Control.Monad (replicateM)
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)
import           Data.Proxy (Proxy (..))
import           Data.SOP.Strict (NP (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Bcc.Crypto.Hash (ShortHash)
import           Bcc.Slotting.Slot (EpochSize (..), SlotNo (..))

import           Shardagnostic.Consensus.BlockchainTime
import           Shardagnostic.Consensus.Config.SecurityParam
import           Shardagnostic.Consensus.Ledger.SupportsMempool (extractTxs)
import           Shardagnostic.Consensus.Node.NetworkProtocolVersion
import           Shardagnostic.Consensus.Node.ProtocolInfo
import           Shardagnostic.Consensus.NodeId

import           Shardagnostic.Consensus.HardFork.Combinator.Serialisation.Common
                     (isHardForkNodeToNodeEnabled)

import qualified Bcc.Ledger.BaseTypes as SL
import qualified Bcc.Protocol.TOptimum.OCert as SL
import qualified Sophie.Spec.Ledger.PParams as SL

import           Shardagnostic.Consensus.Sophie.Eras
import           Shardagnostic.Consensus.Sophie.Node
                     (ProtocolParamsSophieBased (..), SophieGenesis (..))

import           Shardagnostic.Consensus.Bcc.Condense ()
import           Shardagnostic.Consensus.Bcc.Node
                     (ProtocolTransitionParamsSophieBased (..),
                     TriggerHardFork (..))

import           Test.ThreadNet.General
import           Test.ThreadNet.Network (NodeOutput (..),
                     TestNodeInitialization (..))
import           Test.ThreadNet.Util.Expectations (NumBlocks (..))
import           Test.ThreadNet.Util.NodeJoinPlan (trivialNodeJoinPlan)
import           Test.ThreadNet.Util.NodeRestarts (noRestarts)
import           Test.ThreadNet.Util.NodeToNodeVersion (genVersionFiltered)
import           Test.ThreadNet.Util.Seed (runGen)
import qualified Test.Util.BoolProps as BoolProps
import           Test.Util.HardFork.Future (EraSize (..), Future (..))
import           Test.Util.Nightly (askBcccoinNightlyEnabled)
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Slots (NumSlots (..))

import           Test.Consensus.Sophie.MockCrypto (MockCrypto)
import qualified Test.ThreadNet.Infra.Sophie as Sophie
import           Test.ThreadNet.Infra.SophieBasedHardFork
import           Test.ThreadNet.TxGen
import           Test.ThreadNet.TxGen.Evie ()
import           Test.ThreadNet.TxGen.Sophie

import           Test.ThreadNet.Infra.TwoEras

-- | No Cole era, so our crypto can be trivial.
type Crypto = MockCrypto ShortHash

type SophieEvieBlock =
  SophieBasedHardForkBlock (SophieEra Crypto) (EvieEra Crypto)

-- | The varying data of this test
--
-- Note: The Sophie nodes in this test all join, propose an update, and endorse
-- it literally as soon as possible. Therefore, if the test reaches the end of
-- the first epoch, the proposal will be adopted.
data TestSetup = TestSetup
  { setupD            :: Sophie.DecentralizationParam
  , setupHardFork     :: Bool
    -- ^ whether the proposal should trigger a hard fork or not
  , setupInitialNonce :: SL.Nonce
    -- ^ the initial Sophie 'SL.ticknStateEpochNonce'
    --
    -- We vary it to ensure we explore different leader schedules.
  , setupK            :: SecurityParam
  , setupPartition    :: Partition
  , setupSlotLength   :: SlotLength
  , setupTestConfig   :: TestConfig
  , setupVersion      :: (NodeToNodeVersion, BlockNodeToNodeVersion SophieEvieBlock)
  }
  deriving (Show)

instance Arbitrary TestSetup where
  arbitrary = do
    setupD <- arbitrary
                -- The decentralization parameter cannot be 0 in the first
                -- Sophie epoch, since stake pools can only be created and
                -- delegated to via Sophie transactions.
                `suchThat` ((/= 0) . Sophie.decentralizationParamToRational)
    setupK <- SecurityParam <$> choose (8, 10)
                -- If k < 8, common prefix violations become too likely in
                -- Optimum mode for thin overlay schedules (ie low d), even for
                -- f=0.2.

    setupInitialNonce <- genNonce

    setupSlotLength   <- arbitrary

    let epochSize = EpochSize $ sophieEpochSize setupK
    setupTestConfig <- genTestConfig
                         setupK
                         (epochSize, epochSize)
    let TestConfig{numCoreNodes, numSlots} = setupTestConfig

    setupHardFork  <- frequency [(49, pure True), (1, pure False)]

    -- TODO How reliable is the Cole-based partition duration logic when
    -- reused for Sophie?
    setupPartition <- genPartition numCoreNodes numSlots setupK

    setupVersion   <- genVersionFiltered
                        isHardForkNodeToNodeEnabled
                        (Proxy @SophieEvieBlock)

    pure TestSetup
      { setupD
      , setupHardFork
      , setupInitialNonce
      , setupK
      , setupPartition
      , setupSlotLength
      , setupTestConfig
      , setupVersion
      }

  -- TODO shrink

-- | Run relatively fewer tests
--
-- These tests are slow, so we settle for running fewer of them in this test
-- suite since it is invoked frequently (eg CI for each push).
oneTenthTestCount :: QuickCheckTests -> QuickCheckTests
oneTenthTestCount (QuickCheckTests n) = QuickCheckTests $
    if 0 == n then 0 else
    max 1 $ n `div` 10

tests :: TestTree
tests = testGroup "SophieEvie ThreadNet" $
    [ let name = "simple convergence" in
      askBcccoinNightlyEnabled $ \enabled ->
      (if enabled then id else adjustOption oneTenthTestCount) $
      testProperty name $ \setup ->
        prop_simple_sophieEvie_convergence setup
    ]

prop_simple_sophieEvie_convergence :: TestSetup -> Property
prop_simple_sophieEvie_convergence TestSetup
  { setupD
  , setupHardFork
  , setupInitialNonce
  , setupK
  , setupPartition
  , setupSlotLength
  , setupTestConfig
  , setupVersion
  } =
    prop_general_semisync pga testOutput .&&.
    prop_inSync testOutput .&&.
    prop_ReachesEra2 reachesEra2 .&&.
    prop_noCPViolation .&&.
    ( tabulate "ReachesEra2 label" [label_ReachesEra2 reachesEra2] $
      tabulate "Observed forge during a non-overlay slot in the second era"
        [ label_hadActiveNonOverlaySlots
            testOutput
            overlaySlots
        ] $
      tabulatePartitionDuration setupK setupPartition $
      tabulateFinalIntersectionDepth
        setupK
        (NumBlocks finalIntersectionDepth)
        finalBlockEra $
      tabulatePartitionPosition
        (NumSlots numFirstEraSlots)
        setupPartition
        (ledgerReachesEra2 reachesEra2) $
      property True
    )
  where
    TestConfig
      { initSeed
      , numCoreNodes
      , numSlots
      } = setupTestConfig

    pga = PropGeneralArgs
        { pgaBlockProperty       = const $ property True
        , pgaCountTxs            = fromIntegral . length . extractTxs
        , pgaExpectedCannotForge = noExpectedCannotForges
        , pgaFirstBlockNo        = 0
        , pgaFixedMaxForkLength  = Just maxForkLength
          -- the leader schedule isn't fixed because the Sophie leader
          -- schedule is (at least ideally) unpredictable
        , pgaFixedSchedule       = Nothing
        , pgaSecurityParam       = setupK
        , pgaTestConfig          = setupTestConfig
        , pgaTestConfigB         = testConfigB
        }

    txGenExtra = SophieTxGenExtra
      { stgeGenEnv  = mkGenEnv DoNotGeneratePPUs coreNodes
        -- We don't generate any transactions before the transaction
        -- carrying the proposal because they might consume its inputs
        -- before it does, thereby rendering it invalid.
      , stgeStartAt = SlotNo 1
      }

    testConfigB = TestConfigB
      { forgeEbbEnv  = Nothing
      , future       =
          if setupHardFork
          then
          -- In this case the PVU will trigger the transition to the second era
          --
          -- By FACT (B), the PVU is always successful if we reach the second
          -- era.
          EraCons  setupSlotLength epochSize firstEraSize $
          EraFinal setupSlotLength epochSize
          else
          EraFinal setupSlotLength epochSize
      , messageDelay = mkMessageDelay setupPartition
      , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
      , nodeRestarts = noRestarts
      , txGenExtra   = WrapTxGenExtra txGenExtra :* WrapTxGenExtra () :* Nil
      , version      = setupVersion
      }

    testOutput :: TestOutput SophieEvieBlock
    testOutput = runTestNetwork setupTestConfig testConfigB TestConfigMB {
          nodeInfo = \(CoreNodeId nid) ->
            TestNodeInitialization {
                tniCrucialTxs   =
                  if not setupHardFork then [] else
                  fmap GenTxSophie1 $
                  Sophie.mkSetDecentralizationParamTxs
                    coreNodes
                    (SL.ProtVer majorVersion2 0)
                    (SlotNo $ unNumSlots numSlots)   -- never expire
                    setupD   -- unchanged
              , tniProtocolInfo =
                  protocolInfoSophieBasedHardFork
                    ProtocolParamsSophieBased {
                        sophieBasedGenesis           = genesisSophie
                      , sophieBasedInitialNonce      = setupInitialNonce
                      , sophieBasedLeaderCredentials =
                          [Sophie.mkLeaderCredentials
                            (coreNodes !! fromIntegral nid)]
                      }
                    (SL.ProtVer majorVersion1 0)
                    (SL.ProtVer majorVersion2 0)
                    ProtocolTransitionParamsSophieBased {
                        transitionTranslationContext = ()
                      , transitionTrigger            =
                          TriggerHardForkAtVersion majorVersion2
                      }
              }
          , mkRekeyM = Nothing
          }

    maxForkLength :: NumBlocks
    maxForkLength = NumBlocks $ maxRollbacks setupK

    initialKESPeriod :: SL.KESPeriod
    initialKESPeriod = SL.KESPeriod 0

    coreNodes :: [Sophie.CoreNode Crypto]
    coreNodes = runGen initSeed $
        replicateM (fromIntegral n) $
          Sophie.genCoreNode initialKESPeriod
      where
        NumCoreNodes n = numCoreNodes

    maxEntropicSupply :: Word64
    maxEntropicSupply =
      fromIntegral (length coreNodes) * Sophie.initialEntropicPerCoreNode

    genesisSophie :: SophieGenesis (SophieEra Crypto)
    genesisSophie =
        Sophie.mkGenesisConfig
          (SL.ProtVer majorVersion1 0)
          setupK
          activeSlotCoeff
          setupD
          maxEntropicSupply
          setupSlotLength
          (Sophie.mkKesConfig (Proxy @Crypto) numSlots)
          coreNodes

    -- the Sophie ledger is designed to use a fixed epoch size, so this test
    -- does not randomize it
    epochSize :: EpochSize
    epochSize = sgEpochLength genesisSophie

    firstEraSize :: EraSize
    firstEraSize = EraSize numFirstEraEpochs

    -- Classifying test cases

    reachesEra2 :: ReachesEra2
    reachesEra2 = ReachesEra2
      { rsEra1Slots  =
          BoolProps.enabledIf $ t > numFirstEraSlots
      , rsPV         = BoolProps.enabledIf setupHardFork
      , rsEra2Blocks =
          or $
          [ not $ isFirstEraBlock blk
          | (_nid, no) <- Map.toList testOutputNodes
          , let NodeOutput{nodeOutputForges} = no
          , (blk, _m) <- maybeToList $ Map.maxView nodeOutputForges
                -- the last block the node forged
          ]
      , rsEra2Slots  =
          --- TODO this comment and code are wrong

          BoolProps.requiredIf $
          -- The active slots in the first two Sophie epochs are all overlay
          -- slots, so the first Sophie block will arise from one of those.
          not $ Set.null overlaySlots
      }
      where
        NumSlots t                  = numSlots
        TestOutput{testOutputNodes} = testOutput

    -- All OBFT overlay slots in the second era.
    overlaySlots :: Set SlotNo
    overlaySlots =
        secondEraOverlaySlots
          numSlots
          (NumSlots numFirstEraSlots)
          (SL._d (sgProtocolParams genesisSophie))
          epochSize

    numFirstEraSlots :: Word64
    numFirstEraSlots =
        numFirstEraEpochs * unEpochSize epochSize

    finalBlockEra :: String
    finalBlockEra =
        if rsEra2Blocks reachesEra2
        then "Evie"
        else "Sophie"

    finalIntersectionDepth :: Word64
    finalIntersectionDepth = depth
      where
        NumBlocks depth = calcFinalIntersectionDepth pga testOutput

    prop_noCPViolation :: Property
    prop_noCPViolation =
        counterexample
          ( "finalChains: " <>
            show (nodeOutputFinalChain <$> testOutputNodes testOutput)
          ) $
        counterexample "CP violation in final chains!" $
        property $ maxRollbacks setupK >= finalIntersectionDepth

{-------------------------------------------------------------------------------
  Constants
-------------------------------------------------------------------------------}

-- | The major protocol version of the first era in this test
majorVersion1 :: Num a => a
majorVersion1 = 0

-- | The major protocol version of the second era in this test
majorVersion2 :: Num a => a
majorVersion2 = majorVersion1 + 1
