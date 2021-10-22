{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test infrastructure to test hard-forking from one Sophie-based era to
-- another, e.g., Sophie to Evie.
module Test.ThreadNet.Infra.SophieBasedHardFork (
    -- * Blocks
    SophieBasedHardForkBlock
  , SophieBasedHardForkEras
    -- * Transactions
  , pattern GenTxSophie1
  , pattern GenTxSophie2
    -- * Node
  , SophieBasedHardForkConstraints
  , protocolInfoSophieBasedHardFork
  ) where

import           Control.Monad.Except (runExcept)
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict
import           Data.Void (Void)

import           Shardagnostic.Consensus.Ledger.Basics (LedgerConfig)
import           Shardagnostic.Consensus.Node
import           Shardagnostic.Consensus.Node.NetworkProtocolVersion
import           Shardagnostic.Consensus.TypeFamilyWrappers
import           Shardagnostic.Consensus.Util (eitherToMaybe)
import           Shardagnostic.Consensus.Util.IOLike (IOLike)

import           Shardagnostic.Consensus.HardFork.Combinator
import           Shardagnostic.Consensus.HardFork.Combinator.Embed.Binary
import           Shardagnostic.Consensus.HardFork.Combinator.Serialisation
import qualified Shardagnostic.Consensus.HardFork.Combinator.State.Types as HFC
import qualified Shardagnostic.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import qualified Shardagnostic.Consensus.HardFork.Combinator.Util.Tails as Tails
import qualified Shardagnostic.Consensus.HardFork.History as History

import qualified Bcc.Ledger.Era as SL
import qualified Sophie.Spec.Ledger.API as SL

import           Shardagnostic.Consensus.Mempool.TxLimits (TxLimits)
import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits
import           Shardagnostic.Consensus.Sophie.Eras
import           Shardagnostic.Consensus.Sophie.Ledger
import           Shardagnostic.Consensus.Sophie.Node
import           Shardagnostic.Consensus.Sophie.Protocol

import           Shardagnostic.Consensus.Bcc.CanHardFork
                     (SophiePartialLedgerConfig (..), forecastAcrossSophie,
                     translateChainDepStateAcrossSophie)
import           Shardagnostic.Consensus.Bcc.Node
                     (ProtocolTransitionParamsSophieBased (..),
                     TriggerHardFork (..))

import           Test.ThreadNet.TxGen
import           Test.ThreadNet.TxGen.Sophie ()

{-------------------------------------------------------------------------------
  Block type
-------------------------------------------------------------------------------}

-- | Two eras, both Sophie-based.
type SophieBasedHardForkEras era1 era2 =
    '[SophieBlock era1, SophieBlock era2]

type SophieBasedHardForkBlock era1 era2 =
  HardForkBlock (SophieBasedHardForkEras era1 era2)

{-------------------------------------------------------------------------------
  Pattern synonyms, for encapsulation and legibility
-------------------------------------------------------------------------------}

type SophieBasedHardForkGenTx era1 era2 =
  GenTx (SophieBasedHardForkBlock era1 era2)

pattern GenTxSophie1 ::
     GenTx (SophieBlock era1)
  -> SophieBasedHardForkGenTx era1 era2
pattern GenTxSophie1 tx = HardForkGenTx (OneEraGenTx (Z tx))

pattern GenTxSophie2 ::
     GenTx (SophieBlock era2)
  -> SophieBasedHardForkGenTx era1 era2
pattern GenTxSophie2 tx = HardForkGenTx (OneEraGenTx (S (Z tx)))

{-# COMPLETE GenTxSophie1, GenTxSophie2 #-}

pattern SophieBasedHardForkNodeToNodeVersion1 ::
     BlockNodeToNodeVersion (SophieBasedHardForkBlock era1 era2)
pattern SophieBasedHardForkNodeToNodeVersion1 =
    HardForkNodeToNodeEnabled
      HardForkSpecificNodeToNodeVersion1
      (  EraNodeToNodeEnabled SophieNodeToNodeVersion1
      :* EraNodeToNodeEnabled SophieNodeToNodeVersion1
      :* Nil
      )

pattern SophieBasedHardForkNodeToClientVersion1 ::
     BlockNodeToClientVersion (SophieBasedHardForkBlock era1 era2)
pattern SophieBasedHardForkNodeToClientVersion1 =
    HardForkNodeToClientEnabled
      HardForkSpecificNodeToClientVersion2
      (  EraNodeToClientEnabled SophieNodeToClientVersion2
      :* EraNodeToClientEnabled SophieNodeToClientVersion2
      :* Nil
      )

{-------------------------------------------------------------------------------
  Consensus instances
-------------------------------------------------------------------------------}

type SophieBasedHardForkConstraints era1 era2 =
  ( SophieBasedEra era1
  , SophieBasedEra era2
  , TxLimits (SophieBlock era1)
  , TxLimits (SophieBlock era2)
  , EraCrypto era1 ~ EraCrypto era2
  , SL.PreviousEra era2 ~ era1

  , SL.TranslateEra       era2 SL.NewEpochState
  , SL.TranslateEra       era2 SL.SophieGenesis
  , SL.TranslateEra       era2 WrapTx

  , SL.TranslationError   era2 SL.NewEpochState  ~ Void
  , SL.TranslationError   era2 SL.SophieGenesis ~ Void

  , SL.TranslationContext era1 ~ ()
  )

instance SophieBasedHardForkConstraints era1 era2
      => SerialiseHFC (SophieBasedHardForkEras era1 era2)
   -- use defaults

instance SophieBasedHardForkConstraints era1 era2
      => CanHardFork (SophieBasedHardForkEras era1 era2) where
  hardForkEraTranslation = EraTranslation {
        translateLedgerState   = PCons translateLedgerState                PNil
      , translateChainDepState = PCons translateChainDepStateAcrossSophie PNil
      , translateLedgerView    = PCons translateLedgerView                 PNil
      }
    where
      translateLedgerState ::
           InPairs.RequiringBoth
             WrapLedgerConfig
             (HFC.Translate LedgerState)
             (SophieBlock era1)
             (SophieBlock era2)
      translateLedgerState =
          InPairs.RequireBoth
        $ \_cfg1 cfg2 -> HFC.Translate
        $ \_epochNo ->
              unComp
            . SL.translateEra'
                (sophieLedgerTranslationContext (unwrapLedgerConfig cfg2))
            . Comp

      translateLedgerView ::
           InPairs.RequiringBoth
              WrapLedgerConfig
              (HFC.TranslateForecast LedgerState WrapLedgerView)
              (SophieBlock era1)
              (SophieBlock era2)
      translateLedgerView =
          InPairs.RequireBoth $ \(WrapLedgerConfig cfg1) (WrapLedgerConfig cfg2) ->
            HFC.TranslateForecast $ forecastAcrossSophie cfg1 cfg2

  hardForkChainSel = Tails.mk2 SelectSameProtocol

  hardForkInjectTxs =
        InPairs.mk2
      $ InPairs.RequireBoth $ \_cfg1 cfg2 ->
        let ctxt = sophieLedgerTranslationContext (unwrapLedgerConfig cfg2)
        in
          Pair2
            (InjectTx          (translateTx          ctxt))
            (InjectValidatedTx (translateValidatedTx ctxt))
    where
      translateTx ::
           SL.TranslationContext era2
        ->        GenTx (SophieBlock era1)
        -> Maybe (GenTx (SophieBlock era2))
      translateTx transCtxt =
          fmap unComp
        . eitherToMaybe . runExcept . SL.translateEra transCtxt
        . Comp

      translateValidatedTx ::
           SL.TranslationContext era2
        ->        WrapValidatedGenTx (SophieBlock era1)
        -> Maybe (WrapValidatedGenTx (SophieBlock era2))
      translateValidatedTx transCtxt =
            fmap unComp
          . eitherToMaybe . runExcept . SL.translateEra transCtxt
          . Comp

instance SophieBasedHardForkConstraints era1 era2
      => SupportedNetworkProtocolVersion (SophieBasedHardForkBlock era1 era2) where
  supportedNodeToNodeVersions _ = Map.fromList $
      [ (maxBound, SophieBasedHardForkNodeToNodeVersion1)
      ]

  supportedNodeToClientVersions _ = Map.fromList $
      [ (maxBound, SophieBasedHardForkNodeToClientVersion1)
      ]

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

{-------------------------------------------------------------------------------
  Protocol info
-------------------------------------------------------------------------------}

protocolInfoSophieBasedHardFork ::
     forall m era1 era2. (IOLike m, SophieBasedHardForkConstraints era1 era2)
  => ProtocolParamsSophieBased era1
  -> SL.ProtVer
  -> SL.ProtVer
  -> ProtocolTransitionParamsSophieBased era2
  -> ProtocolInfo m (SophieBasedHardForkBlock era1 era2)
protocolInfoSophieBasedHardFork protocolParamsSophieBased
                                 protVer1
                                 protVer2
                                 protocolTransitionParams =
    protocolInfoBinary
      -- Era 1
      protocolInfo1
      eraParams1
      toptimumParams
      toPartialLedgerConfig1
      -- Era 2
      protocolInfo2
      eraParams2
      toptimumParams
      toPartialLedgerConfig2
  where
    ProtocolParamsSophieBased {
        sophieBasedGenesis
      , sophieBasedInitialNonce
      , sophieBasedLeaderCredentials
      } = protocolParamsSophieBased

    -- Era 1

    genesis1 :: SL.SophieGenesis era1
    genesis1 = sophieBasedGenesis

    protocolInfo1 :: ProtocolInfo m (SophieBlock era1)
    protocolInfo1 =
        protocolInfoSophieBased
          protocolParamsSophieBased
          ()  -- trivial translation context
          protVer1
          (TxLimits.mkOverrides TxLimits.noOverridesMeasure)

    eraParams1 :: History.EraParams
    eraParams1 = sophieEraParams genesis1

    ProtocolTransitionParamsSophieBased {
        transitionTranslationContext = transCtxt2
      , transitionTrigger
      } = protocolTransitionParams

    toPartialLedgerConfig1 ::
         LedgerConfig (SophieBlock era1)
      -> PartialLedgerConfig (SophieBlock era1)
    toPartialLedgerConfig1 cfg = SophiePartialLedgerConfig {
          sophieLedgerConfig    = cfg
        , sophieTriggerHardFork = transitionTrigger
        }

    -- Era 2

    genesis2 :: SL.SophieGenesis era2
    genesis2 = SL.translateEra' transCtxt2 genesis1

    protocolInfo2 :: ProtocolInfo m (SophieBlock era2)
    protocolInfo2 =
        protocolInfoSophieBased
          ProtocolParamsSophieBased {
              sophieBasedGenesis = genesis2
            , sophieBasedInitialNonce
            , sophieBasedLeaderCredentials
            }
          transCtxt2
          protVer2
          (TxLimits.mkOverrides TxLimits.noOverridesMeasure)

    eraParams2 :: History.EraParams
    eraParams2 = sophieEraParams genesis2

    toPartialLedgerConfig2 ::
         LedgerConfig (SophieBlock era2)
      -> PartialLedgerConfig (SophieBlock era2)
    toPartialLedgerConfig2 cfg = SophiePartialLedgerConfig {
          sophieLedgerConfig    = cfg
        , sophieTriggerHardFork = TriggerHardForkNever
        }

{-------------------------------------------------------------------------------
  TxGen instance
-------------------------------------------------------------------------------}

-- | Use a generic implementation for 'TxGen'
instance ( TxGen (SophieBlock era1)
         , TxGen (SophieBlock era2)
         , SophieBasedHardForkConstraints era1 era2
         ) => TxGen (SophieBasedHardForkBlock era1 era2) where
  type TxGenExtra (SophieBasedHardForkBlock era1 era2) =
    NP WrapTxGenExtra (SophieBasedHardForkEras era1 era2)
  testGenTxs = testGenTxsHfc
