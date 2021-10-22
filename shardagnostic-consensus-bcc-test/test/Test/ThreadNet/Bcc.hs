{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.ThreadNet.Bcc (tests) where

import           Control.Exception (assert)
import           Control.Monad (replicateM)
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)
import           Data.Proxy (Proxy (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Bcc.Slotting.Slot (EpochSize (..), SlotNo (..))

import           Shardagnostic.Consensus.BlockchainTime
import           Shardagnostic.Consensus.Config.SecurityParam
import           Shardagnostic.Consensus.Ledger.SupportsMempool (extractTxs)
import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits
import           Shardagnostic.Consensus.Node.NetworkProtocolVersion
import           Shardagnostic.Consensus.Node.ProtocolInfo
import           Shardagnostic.Consensus.NodeId
import           Shardagnostic.Consensus.Protocol.PBFT
import           Shardagnostic.Consensus.Util.IOLike (IOLike)

import           Shardagnostic.Consensus.HardFork.Combinator.Serialisation.Common
                     (isHardForkNodeToNodeEnabled)

import qualified Bcc.Chain.Common as CC.Common
import qualified Bcc.Chain.Genesis as CC.Genesis
import           Bcc.Chain.ProtocolConstants (kEpochSlots)
import           Bcc.Chain.Slotting (unEpochSlots)
import qualified Bcc.Chain.Update as CC.Update

import           Shardagnostic.Consensus.Cole.Ledger.Block (ColeBlock)
import           Shardagnostic.Consensus.Cole.Ledger.Conversions
import           Shardagnostic.Consensus.Cole.Node

import qualified Bcc.Ledger.BaseTypes as SL (ActiveSlotCoeff)
import qualified Sophie.Spec.Ledger.API as SL

import           Shardagnostic.Consensus.Sophie.Node

import           Shardagnostic.Consensus.Bcc.Block
import           Shardagnostic.Consensus.Bcc.Condense ()
import           Shardagnostic.Consensus.Bcc.Node

import           Test.Consensus.Bcc.MockCrypto (MockCryptoCompatCole)
import           Test.ThreadNet.General
import qualified Test.ThreadNet.Infra.Aurum as Aurum
import qualified Test.ThreadNet.Infra.Cole as Cole
import qualified Test.ThreadNet.Infra.Sophie as Sophie
import           Test.ThreadNet.Network (NodeOutput (..),
                     TestNodeInitialization (..))
import           Test.ThreadNet.TxGen.Bcc (BccTxGenExtra (..))
import           Test.ThreadNet.Util.Expectations (NumBlocks (..))
import           Test.ThreadNet.Util.NodeJoinPlan (trivialNodeJoinPlan)
import           Test.ThreadNet.Util.NodeRestarts (noRestarts)
import           Test.ThreadNet.Util.NodeToNodeVersion (genVersionFiltered)
import           Test.ThreadNet.Util.Seed (runGen)
import qualified Test.Util.BoolProps as BoolProps
import           Test.Util.HardFork.Future
import           Test.Util.Nightly
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Slots (NumSlots (..))

import           Test.ThreadNet.Infra.TwoEras

-- | Use 'MockCryptoCompatCole' so that bootstrap addresses and
-- bootstrap witnesses are supported.
type Crypto = MockCryptoCompatCole

-- | The varying data of this test
--
-- Note: The Cole nodes in this test all join, propose an update, vote for it,
-- and endorse it literally as soon as possible. Therefore, if the test reaches
-- the end of the first epoch, the proposal will be adopted.
data TestSetup = TestSetup
  { setupD                 :: Sophie.DecentralizationParam
  , setupHardFork          :: Bool
    -- ^ whether the proposal should trigger a hard fork or not
  , setupInitialNonce      :: SL.Nonce
    -- ^ the initial Sophie 'SL.ticknStateEpochNonce'
    --
    -- We vary it to ensure we explore different leader schedules.
  , setupK                 :: SecurityParam
  , setupPartition         :: Partition
  , setupSlotLengthCole   :: SlotLength
  , setupSlotLengthSophie :: SlotLength
  , setupTestConfig        :: TestConfig
  , setupVersion           :: (NodeToNodeVersion, BlockNodeToNodeVersion (BccBlock Crypto))
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

    setupSlotLengthCole   <- arbitrary
    setupSlotLengthSophie <- arbitrary

    setupTestConfig <- genTestConfig
                         setupK
                         ( EpochSize $ coleEpochSize setupK
                         , EpochSize $ sophieEpochSize setupK
                         )

    let TestConfig{numCoreNodes, numSlots} = setupTestConfig

    setupHardFork        <- frequency [(49, pure True), (1, pure False)]
    setupPartition       <- genPartition numCoreNodes numSlots setupK

    setupVersion         <- genVersionFiltered
                              isHardForkNodeToNodeEnabled
                              (Proxy @(BccBlock Crypto))

    pure TestSetup
      { setupD
      , setupHardFork
      , setupInitialNonce
      , setupK
      , setupPartition
      , setupSlotLengthCole
      , setupSlotLengthSophie
      , setupTestConfig
      , setupVersion
      }

  -- TODO shrink

-- | Run relatively fewer tests
--
-- These tests are slow, so we settle for running fewer of them in this test
-- suite since it is invoked frequently (eg CI for each push).
twoFifthsTestCount :: QuickCheckTests -> QuickCheckTests
twoFifthsTestCount (QuickCheckTests n) = QuickCheckTests $
    if 0 == n then 0 else
    max 1 $ (2 * n) `div` 5

tests :: TestTree
tests = testGroup "Bcc ThreadNet" $
    [ let name = "simple convergence" in
      askBcccoinNightlyEnabled $ \enabled ->
      if enabled
      then testProperty name $ \setup ->
             prop_simple_bcc_convergence setup
      else adjustOption twoFifthsTestCount $
           testProperty name $ \setup ->
             prop_simple_bcc_convergence setup
    ]

prop_simple_bcc_convergence :: TestSetup -> Property
prop_simple_bcc_convergence TestSetup
  { setupD
  , setupHardFork
  , setupInitialNonce
  , setupK
  , setupPartition
  , setupSlotLengthCole
  , setupSlotLengthSophie
  , setupTestConfig
  , setupVersion
  } =
    prop_general_semisync pga testOutput .&&.
    prop_inSync testOutput .&&.
    prop_ReachesEra2 reachesEra2 .&&.
    prop_noCPViolation .&&.
    ( tabulate "ReachesEra2 label" [label_ReachesEra2 reachesEra2] $
      tabulate "Observed forge during a non-overlay Sophie slot"
        [label_hadActiveNonOverlaySlots testOutput overlaySlots] $
      tabulatePartitionDuration setupK setupPartition $
      tabulateFinalIntersectionDepth
        setupK
        (NumBlocks finalIntersectionDepth)
        finalBlockEra $
      tabulatePartitionPosition
        (NumSlots numColeSlots)
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
        , pgaFirstBlockNo        = 1
        , pgaFixedMaxForkLength  = Just maxForkLength
        , pgaFixedSchedule       =
            -- the leader schedule isn't fixed because the Sophie leader
            -- schedule is (at least ideally) unpredictable
            Nothing
        , pgaSecurityParam       = setupK
        , pgaTestConfig          = setupTestConfig
        , pgaTestConfigB         = testConfigB
        }

    testConfigB = TestConfigB
      { forgeEbbEnv  = Nothing
      , future       =
          if setupHardFork
          then
          -- In this case the PVU will trigger the transition to Sophie.
          --
          -- By FACT (B), the PVU is always successful if we reach the second
          -- era.
          EraCons  setupSlotLengthCole   epochSizeCole   eraSizeCole $
          EraFinal setupSlotLengthSophie epochSizeSophie
          else
          EraFinal setupSlotLengthCole   epochSizeCole
      , messageDelay = mkMessageDelay setupPartition
      , nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
      , nodeRestarts = noRestarts
      , txGenExtra   = BccTxGenExtra
        { ctgeColeGenesisKeys = generatedSecrets
        , ctgeNetworkMagic     =
            CC.Common.makeNetworkMagic $
            CC.Genesis.configProtocolMagic genesisCole
        , ctgeSophieCoreNodes = coreNodes
        }
      , version      = setupVersion
      }

    testOutput :: TestOutput (BccBlock Crypto)
    testOutput =
        runTestNetwork setupTestConfig testConfigB TestConfigMB
            { nodeInfo = \coreNodeId@(CoreNodeId nid) ->
                mkProtocolBccAndHardForkTxs
                  pbftParams
                  coreNodeId
                  genesisCole
                  generatedSecrets
                  propPV
                  genesisSophie
                  setupInitialNonce
                  (coreNodes !! fromIntegral nid)
                  ProtocolTransitionParamsSophieBased {
                      transitionTranslationContext = ()
                    , transitionTrigger            =
                        TriggerHardForkAtVersion sophieMajorVersion
                    }
            , mkRekeyM = Nothing
            }

    maxForkLength :: NumBlocks
    maxForkLength = NumBlocks $
        if rsEra2Blocks reachesEra2
        then
          -- Sophie inherently creates small forks, but we haven't yet seen a
          -- Common Prefix violation in this test even though @k@ is small
          --
          -- TODO I'd value having a repro that demonstrates a violation of
          -- this typical limit, so I'm leaving it in for now. If it never
          -- fails, we should figure out why not. Even with @k=2 ncn=5 d=0.1@
          -- fixed the deepest fork I'm seeing is ~2.5% @k-1@
          -- 'finalIntersectionDepth'.
          maxRollbacks setupK
        else
          -- Recall that all nodes join ASAP, so the partition is the only
          -- potential cause for a fork during Cole. See the reasoning in
          -- 'genPartition' for the motivation of this limit.
          div partitionDuration 2 + mod partitionDuration 2

    partitionDuration :: Word64
    partitionDuration = dur
      where
        Partition _ (NumSlots dur) = setupPartition

    -- Cole

    pbftParams :: PBftParams
    pbftParams = Cole.colePBftParams setupK numCoreNodes

    -- the Cole ledger is designed to use a fixed epoch size, so this test
    -- does not randomize it
    epochSizeCole :: EpochSize
    epochSizeCole =
        fromColeEpochSlots $ CC.Genesis.configEpochSlots genesisCole

    eraSizeCole :: EraSize
    eraSizeCole = EraSize numFirstEraEpochs

    genesisCole     :: CC.Genesis.Config
    generatedSecrets :: CC.Genesis.GeneratedSecrets
    (genesisCole, generatedSecrets) =
        Cole.generateGenesisConfig setupSlotLengthCole pbftParams

    -- Sophie

    initialKESPeriod :: SL.KESPeriod
    initialKESPeriod = SL.KESPeriod 0

    coreNodes :: [Sophie.CoreNode Crypto]
    coreNodes = runGen initSeed $
        replicateM (fromIntegral n) $
          Sophie.genCoreNode initialKESPeriod
      where
        NumCoreNodes n = numCoreNodes

    -- Same value as for mainnet. Must be larger than the amount of Entropic in
    -- circulation in the Cole ledger. Since this is the maximum value of
    -- entropic, this is guaranteed to be the case.
    maxEntropicSupply :: Word64
    maxEntropicSupply = 45000000000000000

    genesisSophie :: SophieGenesis (SophieEra Crypto)
    genesisSophie =
        Sophie.mkGenesisConfig
          (SL.ProtVer sophieMajorVersion 0)
          setupK
          activeSlotCoeff
          setupD
          maxEntropicSupply
          setupSlotLengthSophie
          (Sophie.mkKesConfig (Proxy @Crypto) numSlots)
          coreNodes

    -- the Sophie ledger is designed to use a fixed epoch size, so this test
    -- does not randomize it
    epochSizeSophie :: EpochSize
    epochSizeSophie = sgEpochLength genesisSophie

    -- the protocol version of the Cole era proposal
    --
    -- FACT (B) This proposal is always adopted at the first epoch boundary.
    --
    -- o 'genTestConfig' ensures the test reaches the epoch boundary unless
    --   there's a fatal error during execution. Specifically, 'rsEra1Slots'
    --   will always be 'Enabled'.
    --
    -- o 'genPartition' limits the partition duration to at most @2k-2@ slots.
    --   This leaves at least @10k - (2k-2) = 8k+2@ slots in the epoch
    --   unaffected by the partition. Moreover, the blocks forged during the
    --   partition do make some progress, even though it's not full progress.
    --   So @8k+2@ is conservative.
    --
    -- o As " crucial transactions ", the proposal and vote are added to the
    --   chain eventually and ASAP, even if they are initially placed on the
    --   losing partition class's chain.
    --
    -- o Thus, within the first two of the @8k+2@ unaffected slots, the
    --   proposal has been confirmed. Similar reasoning ensures that it is then
    --   stably confirmed, endorsed, and stably endorsed, before the epoch
    --   boundary and @SafeZone@. IE @2 + 2k + q + 2k + 2k < 8k+2@, since the
    --   quorum @q@ is ~60% of 'numCoreNodes' and so @q < 2k@, since
    --   'numCoreNodes' is at most 5 and @k@ is at least @2@. (Also recall that
    --   @8k+2@ is conservative.)
    propPV :: CC.Update.ProtocolVersion
    propPV =
      if setupHardFork
      then
        -- this new version must induce the hard fork if accepted
        CC.Update.ProtocolVersion sophieMajorVersion 0 0
      else
        -- this new version must not induce the hard fork if accepted
        CC.Update.ProtocolVersion
          coleMajorVersion (coleInitialMinorVersion + 1) 0

    -- Classifying test cases

    reachesEra2 :: ReachesEra2
    reachesEra2 = ReachesEra2
      { rsEra1Slots    =
          BoolProps.enabledIf $ t > numColeSlots
      , rsPV            = BoolProps.enabledIf setupHardFork
      , rsEra2Blocks =
          or $
          [ not $ isFirstEraBlock blk
          | (_nid, no) <- Map.toList testOutputNodes
          , let NodeOutput{nodeOutputForges} = no
          , (blk, _m) <- maybeToList $ Map.maxView nodeOutputForges
                -- the last block the node forged
          ]
      , rsEra2Slots  =
          assert (w >= k) $
          BoolProps.requiredIf $
          -- The active slots in the first two Sophie epochs are all overlay
          -- slots, so the first Sophie block will arise from one of those.
          not $ Set.null $ overlaySlots
      }
      where
        NumSlots t                  = numSlots
        TestOutput{testOutputNodes} = testOutput

        k :: Word64
        k = maxRollbacks setupK

        coeff :: SL.ActiveSlotCoeff
        coeff = SL.sgActiveSlotCoeff genesisSophie

        w :: Word64
        w = SL.computeStabilityWindow k coeff

    overlaySlots :: Set SlotNo
    overlaySlots =
        secondEraOverlaySlots
          numSlots
          (NumSlots numColeSlots)
          (SL._d (sgProtocolParams genesisSophie))
          epochSizeSophie

    numColeSlots :: Word64
    numColeSlots = numFirstEraEpochs * unEpochSize epochSizeCole

    finalBlockEra :: String
    finalBlockEra =
        if rsEra2Blocks reachesEra2 then "Sophie" else "Cole"

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

mkProtocolBccAndHardForkTxs
  :: forall c m. (IOLike m, BccHardForkConstraints c)
     -- Cole
  => PBftParams
  -> CoreNodeId
  -> CC.Genesis.Config
  -> CC.Genesis.GeneratedSecrets
  -> CC.Update.ProtocolVersion
     -- Sophie
  -> SophieGenesis (SophieEra c)
  -> SL.Nonce
  -> Sophie.CoreNode c
     -- HardForks
  -> ProtocolTransitionParamsSophieBased (SophieEra c)
  -> TestNodeInitialization m (BccBlock c)
mkProtocolBccAndHardForkTxs
    pbftParams coreNodeId genesisCole generatedSecretsCole propPV
    genesisSophie initialNonce coreNodeSophie
    protocolParamsColeSophie =
    TestNodeInitialization
      { tniCrucialTxs   = crucialTxs
      , tniProtocolInfo = pInfo
      }
  where
    crucialTxs :: [GenTx (BccBlock c)]
    crucialTxs =
        GenTxCole <$> tniCrucialTxs tniCole
      where
        -- reuse the Cole logic for generating the crucial txs, ie the
        -- proposal and votes
        tniCole :: TestNodeInitialization m ColeBlock
        tniCole =
            Cole.mkProtocolColeAndHardForkTxs
              pbftParams
              coreNodeId
              genesisCole
              generatedSecretsCole
              propPV

    pInfo :: ProtocolInfo m (BccBlock c)
    pInfo = protocolInfoBcc
        ProtocolParamsCole {
            coleGenesis                = genesisCole
            -- Trivialize the PBFT signature window so that the forks induced by
            -- the network partition are as deep as possible.
          , colePbftSignatureThreshold = Just $ PBftSignatureThreshold 1
          , coleProtocolVersion        = propPV
          , coleSoftwareVersion        = softVerCole
          , coleLeaderCredentials      = Just leaderCredentialsCole
          , coleMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
          }
        ProtocolParamsSophieBased {
            sophieBasedGenesis           = genesisSophie
          , sophieBasedInitialNonce      = initialNonce
          , sophieBasedLeaderCredentials = [leaderCredentialsSophie]
          }
        ProtocolParamsSophie {
            sophieProtVer                = SL.ProtVer sophieMajorVersion 0
          , sophieMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
          }
        ProtocolParamsEvie {
            evieProtVer                = SL.ProtVer evieMajorVersion 0
          , evieMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
          }
        ProtocolParamsJen {
            jenProtVer                   = SL.ProtVer jenMajorVersion    0
          , jenMaxTxCapacityOverrides    = TxLimits.mkOverrides TxLimits.noOverridesMeasure
          }
        ProtocolParamsAurum {
            aurumProtVer                 = SL.ProtVer aurumMajorVersion  0
          , aurumMaxTxCapacityOverrides  = TxLimits.mkOverrides TxLimits.noOverridesMeasure
          }
        protocolParamsColeSophie
        ProtocolTransitionParamsSophieBased {
            transitionTranslationContext = ()
          , transitionTrigger            =
              TriggerHardForkAtVersion evieMajorVersion
          }
        ProtocolTransitionParamsSophieBased {
            transitionTranslationContext = ()
          , transitionTrigger            =
              TriggerHardForkAtVersion jenMajorVersion
          }
        ProtocolTransitionParamsSophieBased {
            transitionTranslationContext = Aurum.degenerateAurumGenesis
          , transitionTrigger            =
              TriggerHardForkAtVersion aurumMajorVersion
          }

    -- Cole

    leaderCredentialsCole :: ColeLeaderCredentials
    leaderCredentialsCole =
        Cole.mkLeaderCredentials
          genesisCole
          generatedSecretsCole
          coreNodeId

    -- this sets a vestigial header field which is not actually used for anything
    softVerCole :: CC.Update.SoftwareVersion
    softVerCole = Cole.theProposedSoftwareVersion

    -- Sophie

    leaderCredentialsSophie :: TOptimumLeaderCredentials c
    leaderCredentialsSophie = Sophie.mkLeaderCredentials coreNodeSophie

{-------------------------------------------------------------------------------
  Constants
-------------------------------------------------------------------------------}

-- | The major protocol version of Cole in this test
--
-- On mainnet, the Cole era spans multiple major versions: 0 for Classic and 1
-- for OBFT. So Sophie is 2. But in this test, we start with OBFT as major
-- version 0: the nodes are running OBFT from slot 0 and the Cole ledger
-- defaults to an initial version of 0. So Sophie is 1 in this test.
coleMajorVersion :: Num a => a
coleMajorVersion = 0

-- | The major protocol version of Sophie in this test
--
-- See 'coleMajorVersion'
sophieMajorVersion :: Num a => a
sophieMajorVersion = coleMajorVersion + 1

-- | The major protocol version of Evie in this test
--
-- See 'coleMajorVersion'
evieMajorVersion :: Num a => a
evieMajorVersion = sophieMajorVersion + 1

-- | The major protocol version of Jen in this test
--
-- See 'coleMajorVersion'
jenMajorVersion :: Num a => a
jenMajorVersion = evieMajorVersion + 1

-- | The major protocol version of Aurum in this test
--
aurumMajorVersion :: Num a => a
aurumMajorVersion = jenMajorVersion + 1

-- | The initial minor protocol version of Cole in this test
--
-- See 'coleMajorVersion'
coleInitialMinorVersion :: Num a => a
coleInitialMinorVersion = 0

{-------------------------------------------------------------------------------
  Miscellany
-------------------------------------------------------------------------------}

coleEpochSize :: SecurityParam -> Word64
coleEpochSize (SecurityParam k) =
    unEpochSlots $ kEpochSlots $ CC.Common.BlockCount k
