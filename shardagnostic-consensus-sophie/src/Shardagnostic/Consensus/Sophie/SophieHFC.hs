{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module is the Sophie Hard Fork Combinator
module Shardagnostic.Consensus.Sophie.SophieHFC (
    ProtocolSophie
  , SophieBlockHFC
  , SophiePartialLedgerConfig (..)
  , forecastAcrossSophie
  , translateChainDepStateAcrossSophie
  , translateLedgerViewAcrossSophie
  ) where

import           Control.Monad (guard)
import           Control.Monad.Except (runExcept, throwError, withExceptT)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.SOP.Strict
import qualified Data.Text as T (pack)
import           Data.Void (Void)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Bcc.Slotting.EpochInfo (hoistEpochInfo)

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Config
import           Shardagnostic.Consensus.Forecast
import           Shardagnostic.Consensus.HardFork.Combinator
import           Shardagnostic.Consensus.HardFork.Combinator.Serialisation.Common
import           Shardagnostic.Consensus.HardFork.Combinator.State.Types
import           Shardagnostic.Consensus.HardFork.Combinator.Util.InPairs
                     (RequiringBoth (..), ignoringBoth)
import           Shardagnostic.Consensus.HardFork.History (Bound (boundSlot))
import           Shardagnostic.Consensus.HardFork.Simple
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.Node.NetworkProtocolVersion
import           Shardagnostic.Consensus.TypeFamilyWrappers

import qualified Bcc.Ledger.Era as SL
import qualified Sophie.Spec.Ledger.API as SL

import           Shardagnostic.Consensus.Sophie.Eras
import           Shardagnostic.Consensus.Sophie.Ledger
import           Shardagnostic.Consensus.Sophie.Ledger.Inspect as Sophie.Inspect
import           Shardagnostic.Consensus.Sophie.Node ()
import           Shardagnostic.Consensus.Sophie.Protocol

{-------------------------------------------------------------------------------
  Synonym for convenience
-------------------------------------------------------------------------------}

-- | Sophie as the single era in the hard fork combinator
type SophieBlockHFC era = HardForkBlock '[SophieBlock era]

{-------------------------------------------------------------------------------
  NoHardForks instance
-------------------------------------------------------------------------------}

instance SophieBasedEra era => NoHardForks (SophieBlock era) where
  getEraParams =
        sophieEraParamsNeverHardForks
      . sophieLedgerGenesis
      . configLedger
  toPartialConsensusConfig _  = toptimumParams
  toPartialLedgerConfig _ cfg = SophiePartialLedgerConfig {
        sophieLedgerConfig    = cfg
      , sophieTriggerHardFork = TriggerHardForkNever
      }

{-------------------------------------------------------------------------------
  SupportedNetworkProtocolVersion instance
-------------------------------------------------------------------------------}

-- | Forward to the SophieBlock instance. Only supports
-- 'HardForkNodeToNodeDisabled', which is compatible with nodes running with
-- 'SophieBlock'.
instance SophieBasedEra era
      => SupportedNetworkProtocolVersion (SophieBlockHFC era) where
  supportedNodeToNodeVersions _ =
      Map.map HardForkNodeToNodeDisabled $
      supportedNodeToNodeVersions (Proxy @(SophieBlock era))

  supportedNodeToClientVersions _ =
      Map.map HardForkNodeToClientDisabled $
      supportedNodeToClientVersions (Proxy @(SophieBlock era))

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

{-------------------------------------------------------------------------------
  SerialiseHFC instance
-------------------------------------------------------------------------------}

-- | Use the default implementations. This means the serialisation of blocks
-- includes an era wrapper. Each block should do this from the start to be
-- prepared for future hard forks without having to do any bit twiddling.
instance SophieBasedEra era => SerialiseHFC '[SophieBlock era]
instance SophieBasedEra era => SerialiseConstraintsHFC (SophieBlock era)

{-------------------------------------------------------------------------------
  Protocol type definition
-------------------------------------------------------------------------------}

type ProtocolSophie = HardForkProtocol '[ SophieBlock StandardSophie ]

{-------------------------------------------------------------------------------
  SingleEraBlock Sophie
-------------------------------------------------------------------------------}

sophieTransition ::
     forall era. SophieBasedEra era
  => PartialLedgerConfig (SophieBlock era)
  -> Word16   -- ^ Next era's major protocol version
  -> LedgerState (SophieBlock era)
  -> Maybe EpochNo
sophieTransition SophiePartialLedgerConfig{..}
                  transitionMajorVersion
                  state =
      takeAny
    . mapMaybe isTransition
    . Sophie.Inspect.protocolUpdates genesis
    $ state
  where
    SophieTransitionInfo{..} = sophieLedgerTransition state

    -- 'sophieLedgerConfig' contains a dummy 'EpochInfo' but this does not
    -- matter for extracting the genesis config
    genesis :: SL.SophieGenesis era
    genesis = sophieLedgerGenesis sophieLedgerConfig

    k :: Word64
    k = SL.sgSecurityParam genesis

    isTransition :: Sophie.Inspect.ProtocolUpdate era -> Maybe EpochNo
    isTransition Sophie.Inspect.ProtocolUpdate{..} = do
         SL.ProtVer major _seal <- proposalVersion
         guard $ fromIntegral major == transitionMajorVersion
         guard $ proposalReachedQuorum
         guard $ sophieAfterVoting >= fromIntegral k
         return proposalEpoch
       where
         Sophie.Inspect.UpdateProposal{..} = protocolUpdateProposal
         Sophie.Inspect.UpdateState{..}    = protocolUpdateState

    -- In principle there could be multiple proposals that all change the
    -- major protocol version. In practice this can't happen because each
    -- delegate can only vote for one proposal, but the types don't guarantee
    -- this. We don't need to worry about this, and just pick any of them.
    takeAny :: [a] -> Maybe a
    takeAny = listToMaybe

instance SophieBasedEra era => SingleEraBlock (SophieBlock era) where
  singleEraTransition pcfg _eraParams _eraStart ledgerState =
      case sophieTriggerHardFork pcfg of
        TriggerHardForkNever                         -> Nothing
        TriggerHardForkAtEpoch   epoch               -> Just epoch
        TriggerHardForkAtVersion sophieMajorVersion ->
            sophieTransition
              pcfg
              sophieMajorVersion
              ledgerState

  singleEraInfo _ = SingleEraInfo {
      singleEraName = sophieBasedEraName (Proxy @era)
    }

instance OptimumCrypto c => HasPartialConsensusConfig (TOptimum c) where
  type PartialConsensusConfig (TOptimum c) = TOptimumParams

  completeConsensusConfig _ toptimumEpochInfo toptimumParams = TOptimumConfig {..}

data SophiePartialLedgerConfig era = SophiePartialLedgerConfig {
      -- | We cache the non-partial ledger config containing a dummy
      -- 'EpochInfo' that needs to be replaced with the correct one.
      --
      -- We do this to avoid recomputing the ledger config each time
      -- 'completeLedgerConfig' is called, as 'mkSophieLedgerConfig' does
      -- some rather expensive computations that shouldn't be repeated too
      -- often (e.g., 'sgActiveSlotCoeff').
      sophieLedgerConfig    :: !(SophieLedgerConfig era)
    , sophieTriggerHardFork :: !TriggerHardFork
    }
  deriving (Generic, NoThunks)

instance SophieBasedEra era => HasPartialLedgerConfig (SophieBlock era) where
  type PartialLedgerConfig (SophieBlock era) = SophiePartialLedgerConfig era

  -- Replace the dummy 'EpochInfo' with the real one
  completeLedgerConfig _ epochInfo (SophiePartialLedgerConfig cfg _) =
      cfg {
          sophieLedgerGlobals = (sophieLedgerGlobals cfg) {
              SL.epochInfoWithErr =
                  hoistEpochInfo
                    (runExcept . withExceptT (T.pack . show))
                    epochInfo
            }
        }

-- | Forecast from a Sophie-based era to the next Sophie-based era.
forecastAcrossSophie ::
     forall eraFrom eraTo.
     ( EraCrypto eraFrom ~ EraCrypto eraTo
     , SophieBasedEra eraFrom
     )
  => SophieLedgerConfig eraFrom
  -> SophieLedgerConfig eraTo
  -> Bound  -- ^ Transition between the two eras
  -> SlotNo -- ^ Forecast for this slot
  -> LedgerState (SophieBlock eraFrom)
  -> Except OutsideForecastRange (Ticked (WrapLedgerView (SophieBlock eraTo)))
forecastAcrossSophie cfgFrom cfgTo transition forecastFor ledgerStateFrom
    | forecastFor < maxFor
    = return $ futureLedgerView forecastFor
    | otherwise
    = throwError $ OutsideForecastRange {
          outsideForecastAt     = ledgerTipSlot ledgerStateFrom
        , outsideForecastMaxFor = maxFor
        , outsideForecastFor    = forecastFor
        }
  where
    -- | 'SL.futureLedgerView' imposes its own bounds. Those bounds could
    -- /exceed/ the 'maxFor' we have computed, but should never be /less/.
    futureLedgerView :: SlotNo -> Ticked (WrapLedgerView (SophieBlock eraTo))
    futureLedgerView =
          WrapTickedLedgerView
        . TickedOptimumLedgerView
        . either
            (\e -> error ("futureLedgerView failed: " <> show e))
            id
        . SL.futureLedgerView
            (sophieLedgerGlobals cfgFrom)
            (sophieLedgerState ledgerStateFrom)

    -- Exclusive upper bound
    maxFor :: SlotNo
    maxFor = crossEraForecastBound
               (ledgerTipSlot ledgerStateFrom)
               (boundSlot transition)
               (SL.stabilityWindow (sophieLedgerGlobals cfgFrom))
               (SL.stabilityWindow (sophieLedgerGlobals cfgTo))

translateChainDepStateAcrossSophie ::
     forall eraFrom eraTo.
     EraCrypto eraFrom ~ EraCrypto eraTo
  => RequiringBoth
       WrapConsensusConfig
       (Translate WrapChainDepState)
       (SophieBlock eraFrom)
       (SophieBlock eraTo)
translateChainDepStateAcrossSophie =
    ignoringBoth $
      Translate $ \_epochNo (WrapChainDepState chainDepState) ->
        -- Same protocol, same 'ChainDepState'. Note that we don't have to apply
        -- any changes related to an epoch transition, this is already done when
        -- ticking the state.
        WrapChainDepState chainDepState

translateLedgerViewAcrossSophie ::
     forall eraFrom eraTo.
     ( EraCrypto eraFrom ~ EraCrypto eraTo
     , SophieBasedEra eraFrom
     )
  => RequiringBoth
       WrapLedgerConfig
       (TranslateForecast LedgerState WrapLedgerView)
       (SophieBlock eraFrom)
       (SophieBlock eraTo)
translateLedgerViewAcrossSophie =
    RequireBoth $ \(WrapLedgerConfig cfgFrom)
                   (WrapLedgerConfig cfgTo) ->
      TranslateForecast $ forecastAcrossSophie cfgFrom cfgTo

{-------------------------------------------------------------------------------
  Translation from one Sophie-based era to another Sophie-based era
-------------------------------------------------------------------------------}

instance ( SophieBasedEra era
         , SophieBasedEra (SL.PreviousEra era)
         , EraCrypto (SL.PreviousEra era) ~ EraCrypto era
         ) => SL.TranslateEra era SophieTip where
  translateEra _ (SophieTip sno bno (SophieHash hash)) =
      return $ SophieTip sno bno (SophieHash hash)

instance ( SophieBasedEra era
         , SL.TranslateEra era SophieTip
         , SL.TranslateEra era SL.NewEpochState
         , SL.TranslationError era SL.NewEpochState ~ Void
         ) => SL.TranslateEra era (LedgerState :.: SophieBlock) where
  translateEra ctxt (Comp (SophieLedgerState tip state _transition)) = do
      tip'   <- mapM (SL.translateEra ctxt) tip
      state' <- SL.translateEra ctxt state
      return $ Comp $ SophieLedgerState {
          sophieLedgerTip        = tip'
        , sophieLedgerState      = state'
        , sophieLedgerTransition = SophieTransitionInfo 0
        }

instance ( SophieBasedEra era
         , SL.TranslateEra era WrapTx
         ) => SL.TranslateEra era (GenTx :.: SophieBlock) where
  type TranslationError era (GenTx :.: SophieBlock) = SL.TranslationError era WrapTx
  translateEra ctxt (Comp (SophieTx _txId tx)) =
        Comp . mkSophieTx . unwrapTx @era
    <$> SL.translateEra ctxt (WrapTx @(SL.PreviousEra era) tx)

instance ( SophieBasedEra era
         , SL.TranslateEra era WrapTx
         ) => SL.TranslateEra era (WrapValidatedGenTx :.: SophieBlock) where
  type TranslationError era (WrapValidatedGenTx :.: SophieBlock) = SL.TranslationError era WrapTx
  translateEra ctxt (Comp (WrapValidatedGenTx (SophieValidatedTx _txId vtx))) =
        Comp . WrapValidatedGenTx
      . mkSophieValidatedTx . SL.coerceValidated
    <$> SL.translateValidated @era @WrapTx ctxt (SL.coerceValidated vtx)
