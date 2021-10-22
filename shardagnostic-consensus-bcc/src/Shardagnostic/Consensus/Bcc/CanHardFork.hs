{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Shardagnostic.Consensus.Bcc.CanHardFork (
    ColePartialLedgerConfig (..)
  , BccHardForkConstraints
  , TriggerHardFork (..)
    -- * Re-exports of Sophie code
  , SophiePartialLedgerConfig (..)
  , forecastAcrossSophie
  , translateChainDepStateAcrossSophie
  ) where

import           Control.Monad
import           Control.Monad.Except (runExcept, throwError)
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe, mapMaybe)
import           Data.Proxy
import           Data.SOP.Strict (NP (..), unComp, (:.:) (..))
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Bcc.Crypto.DSIGN (Ed25519DSIGN)
import           Bcc.Crypto.Hash.Blake2b (Blake2b_224, Blake2b_256)

import qualified Bcc.Chain.Common as CC
import qualified Bcc.Chain.Genesis as CC.Genesis
import qualified Bcc.Chain.Update as CC.Update

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Forecast
import           Shardagnostic.Consensus.HardFork.History (Bound (boundSlot),
                     addSlots)
import           Shardagnostic.Consensus.HardFork.Simple
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.TypeFamilyWrappers
import           Shardagnostic.Consensus.Util (eitherToMaybe)
import           Shardagnostic.Consensus.Util.RedundantConstraints

import           Shardagnostic.Consensus.HardFork.Combinator
import           Shardagnostic.Consensus.HardFork.Combinator.State.Types
import           Shardagnostic.Consensus.HardFork.Combinator.Util.InPairs
                     (RequiringBoth (..), ignoringBoth)
import           Shardagnostic.Consensus.HardFork.Combinator.Util.Tails (Tails (..))

import           Shardagnostic.Consensus.Cole.Ledger
import qualified Shardagnostic.Consensus.Cole.Ledger.Inspect as Cole.Inspect
import           Shardagnostic.Consensus.Cole.Node ()
import           Shardagnostic.Consensus.Protocol.PBFT (PBft, PBftCrypto)
import           Shardagnostic.Consensus.Protocol.PBFT.State (PBftState)
import qualified Shardagnostic.Consensus.Protocol.PBFT.State as PBftState

import           Shardagnostic.Consensus.Sophie.Ledger
import           Shardagnostic.Consensus.Sophie.Node ()
import           Shardagnostic.Consensus.Sophie.Protocol
import           Shardagnostic.Consensus.Sophie.SophieHFC

import           Bcc.Ledger.Evie.Translation ()
import qualified Bcc.Ledger.Aurum.Genesis as Aurum
import           Bcc.Ledger.Crypto (ADDRHASH, DSIGN, HASH)
import qualified Bcc.Ledger.Era as SL
import           Bcc.Ledger.Jen.Translation ()
import qualified Sophie.Spec.Ledger.API as SL

import           Shardagnostic.Consensus.Bcc.Block

{-------------------------------------------------------------------------------
  Figure out the transition point for Cole

  The Cole ledger defines the update 'State' in
  "Bcc.Chain.Update.Validation.Interface". The critical piece of state we
  need is

  > candidateProtocolUpdates :: ![CandidateProtocolUpdate]

  which are the update proposals that have been voted on, accepted, and
  endorsed, and now need to become stable. In `tryBumpVersion`
  ("Bcc.Chain.Update.Validation.Interface.ProtocolVersionBump") we
  find the candidates that are at least 'kUpdateStabilityParam' (@== 4k@) deep,
  and then construct

  > State
  > { nextProtocolVersion    = cpuProtocolVersion
  > , nextProtocolParameters = cpuProtocolParameters
  > }

  (with 'State' from "Bcc.Chain.Update.Validation.Interface.ProtocolVersionBump")
  where 'cpuProtocolVersion'/'cpuProtocolParameters' are the version and
  parameters from the update. This then ends up in the following callstack

  > applyChainTick
  > |
  > \-- epochTransition
  >     |
  >     \-- registerEpoch
  >         |
  >         \-- tryBumpVersion

  Now, if this is changing the major version of the protocol, then this actually
  indicates the transition to Sophie, and the Cole 'applyChainTick' won't
  actually happen. Instead, in 'singleEraTransition' we will report the
  'EpochNo' of the transition as soon as it's @2k@ (not @4k@!) deep: in other
  words, as soon as it is stable; at this point, the HFC will do the rest.

  A slightly subtle point is that the Cole ledger does not record any
  information about /past/ updates to the protocol parameters, and so if we
  /were/ to ask the Cole ledger /after/ the update when the transition is
  going to take place (did take place), it will say 'Nothing': transition not
  yet known. In practice this won't matter, as it will have been translated to
  a Sophie ledger at that point.
-------------------------------------------------------------------------------}

coleTransition :: PartialLedgerConfig ColeBlock
                -> Word16   -- ^ Sophie major protocol version
                -> LedgerState ColeBlock
                -> Maybe EpochNo
coleTransition ColePartialLedgerConfig{..} sophieMajorVersion state =
      takeAny
    . mapMaybe isTransitionToSophie
    . Cole.Inspect.protocolUpdates coleLedgerConfig
    $ state
  where
    ColeTransitionInfo transitionInfo = coleLedgerTransition state

    genesis = coleLedgerConfig
    k       = CC.Genesis.gdK $ CC.Genesis.configGenesisData genesis

    isTransitionToSophie :: Cole.Inspect.ProtocolUpdate -> Maybe EpochNo
    isTransitionToSophie update = do
        guard $ CC.Update.pvMajor version == sophieMajorVersion
        case Cole.Inspect.protocolUpdateState update of
          Cole.Inspect.UpdateCandidate _becameCandidateSlotNo adoptedIn -> do
            becameCandidateBlockNo <- Map.lookup version transitionInfo
            guard $ isReallyStable becameCandidateBlockNo
            return adoptedIn
          Cole.Inspect.UpdateStableCandidate adoptedIn ->
            -- If the Cole ledger thinks it's stable, it's _definitely_ stable
            return adoptedIn
          _otherwise ->
            -- The proposal isn't yet a candidate, never mind a stable one
            mzero
      where
        version :: CC.Update.ProtocolVersion
        version = Cole.Inspect.protocolUpdateVersion update

    -- Normally, stability in the ledger is defined in terms of slots, not
    -- blocks. Cole considers the proposal to be stable after the slot is more
    -- than @2k@ old. That is not wrong: after @2k@, the block indeed is stable.
    --
    -- Unfortunately, this means that the /conclusion about stability itself/
    -- is /not/ stable: if we were to switch to a denser fork, we might change
    -- our mind (on the sparse chain we thought the block was already stable,
    -- but on the dense chain we conclude it is it not yet stable).
    --
    -- It is unclear at the moment if this presents a problem; the HFC assumes
    -- monotonicity of timing info, in the sense that that any slot/time
    -- conversions are either unknown or else not subject to rollback.
    -- The problem sketched above might mean that we can go from "conversion
    -- known" to "conversion unknown", but then when we go back again to
    -- "conversion known", we /are/ guaranteed that we'd get the same answer.
    --
    -- Rather than trying to analyse this subtle problem, we instead base
    -- stability on block numbers; after the block is `k` deep, we know for sure
    -- that it is stable, and moreover, no matter which chain we switch to, that
    -- will remain to be the case.
    --
    -- The Cole 'UpdateState' records the 'SlotNo' of the block in which the
    -- proposal became a candidate (i.e., when the last required endorsement
    -- came in). That doesn't tell us very much, we need to know the block
    -- number; that's precisely what the 'ColeTransition' part of the Cole
    -- state tells us.
    isReallyStable :: BlockNo -> Bool
    isReallyStable (BlockNo bno) = distance >= CC.unBlockCount k
      where
        distance :: Word64
        distance = case coleLedgerTipBlockNo state of
                     Origin                  -> bno + 1
                     NotOrigin (BlockNo tip) -> tip - bno

    -- We only expect a single proposal that updates to Sophie, but in case
    -- there are multiple, any one will do
    takeAny :: [a] -> Maybe a
    takeAny = listToMaybe

{-------------------------------------------------------------------------------
  SingleEraBlock Cole
-------------------------------------------------------------------------------}

instance SingleEraBlock ColeBlock where
  singleEraTransition pcfg _eraParams _eraStart ledgerState =
      case coleTriggerHardFork pcfg of
        TriggerHardForkNever                         -> Nothing
        TriggerHardForkAtEpoch   epoch               -> Just epoch
        TriggerHardForkAtVersion sophieMajorVersion ->
            coleTransition
              pcfg
              sophieMajorVersion
              ledgerState

  singleEraInfo _ = SingleEraInfo {
      singleEraName = "Cole"
    }

instance PBftCrypto bc => HasPartialConsensusConfig (PBft bc)
  -- Use defaults

-- | When Cole is part of the hard-fork combinator, we use the partial ledger
-- config. Standalone Cole uses the regular ledger config. This means that
-- the partial ledger config is the perfect place to store the trigger
-- condition for the hard fork to Sophie, as we don't have to modify the
-- ledger config for standalone Cole.
data ColePartialLedgerConfig = ColePartialLedgerConfig {
      coleLedgerConfig    :: !(LedgerConfig ColeBlock)
    , coleTriggerHardFork :: !TriggerHardFork
    }
  deriving (Generic, NoThunks)

instance HasPartialLedgerConfig ColeBlock where

  type PartialLedgerConfig ColeBlock = ColePartialLedgerConfig

  completeLedgerConfig _ _ = coleLedgerConfig

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

type BccHardForkConstraints c =
  ( OptimumCrypto c
  , SophieBasedEra (SophieEra c)
  , SophieBasedEra (EvieEra c)
  , SophieBasedEra (JenEra    c)
  , SophieBasedEra (AurumEra  c)
    -- These equalities allow the transition from Cole to Sophie, since
    -- @sophie-spec-ledger@ requires Ed25519 for Cole bootstrap addresses and
    -- the current Cole-to-Sophie translation requires a 224-bit hash for
    -- address and a 256-bit hash for header hashes.
  , HASH     c ~ Blake2b_256
  , ADDRHASH c ~ Blake2b_224
  , DSIGN    c ~ Ed25519DSIGN
  )

instance BccHardForkConstraints c => CanHardFork (BccEras c) where
  hardForkEraTranslation = EraTranslation {
      translateLedgerState   =
          PCons translateLedgerStateColeToSophieWrapper
        $ PCons translateLedgerStateSophieToEvieWrapper
        $ PCons translateLedgerStateEvieToJenWrapper
        $ PCons translateLedgerStateJenToAurumWrapper
        $ PNil
    , translateChainDepState =
          PCons translateChainDepStateColeToSophieWrapper
        $ PCons translateChainDepStateAcrossSophie
        $ PCons translateChainDepStateAcrossSophie
        $ PCons translateChainDepStateAcrossSophie
        $ PNil
    , translateLedgerView    =
          PCons translateLedgerViewColeToSophieWrapper
        $ PCons translateLedgerViewAcrossSophie
        $ PCons translateLedgerViewAcrossSophie
        $ PCons translateLedgerViewAcrossSophie
        $ PNil
    }
  hardForkChainSel =
        -- Cole <-> Sophie, ...
        TCons (   CompareBlockNo
               :* CompareBlockNo
               :* CompareBlockNo
               :* CompareBlockNo
               :* Nil)
        -- Sophie <-> Evie, ...
      $ TCons (SelectSameProtocol :* SelectSameProtocol :* SelectSameProtocol :* Nil)
        -- Evie <-> Jen, ...
      $ TCons (SelectSameProtocol :* SelectSameProtocol :* Nil)
        -- Jen <-> Aurum, ...
      $ TCons (SelectSameProtocol :* Nil)
        -- Aurum <-> ...
      $ TCons Nil
      $ TNil
  hardForkInjectTxs =
        PCons (ignoringBoth $ Pair2 cannotInjectTx cannotInjectValidatedTx)
      $ PCons (   ignoringBoth
                $ Pair2
                    translateTxSophieToEvieWrapper
                    translateValidatedTxSophieToEvieWrapper
              )
      $ PCons (   ignoringBoth
                $ Pair2
                    translateTxEvieToJenWrapper
                    translateValidatedTxEvieToJenWrapper
              )
      $ PCons (RequireBoth $ \_cfgJen cfgAurum ->
                 let ctxt = getAurumTranslationContext cfgAurum
                 in
                 Pair2
                   (translateTxJenToAurumWrapper          ctxt)
                   (translateValidatedTxJenToAurumWrapper ctxt)
              )
      $ PNil

{-------------------------------------------------------------------------------
  Translation from Cole to Sophie
-------------------------------------------------------------------------------}

translateHeaderHashColeToSophie ::
     forall c.
     ( SophieBasedEra (SophieEra c)
     , HASH c ~ Blake2b_256
     )
  => HeaderHash ColeBlock
  -> HeaderHash (SophieBlock (SophieEra c))
translateHeaderHashColeToSophie =
      fromShortRawHash (Proxy @(SophieBlock (SophieEra c)))
    . toShortRawHash   (Proxy @ColeBlock)
  where
    -- Cole uses 'Blake2b_256' for header hashes
    _ = keepRedundantConstraint (Proxy @(HASH c ~ Blake2b_256))

translatePointColeToSophie ::
     ( SophieBasedEra (SophieEra c)
     , HASH c ~ Blake2b_256
     )
  => Point ColeBlock
  -> WithOrigin BlockNo
  -> WithOrigin (SophieTip (SophieEra c))
translatePointColeToSophie point bNo =
    case (point, bNo) of
      (GenesisPoint, Origin) ->
        Origin
      (BlockPoint s h, NotOrigin n) -> NotOrigin SophieTip {
          sophieTipSlotNo  = s
        , sophieTipBlockNo = n
        , sophieTipHash    = translateHeaderHashColeToSophie h
        }
      _otherwise ->
        error "translatePointColeToSophie: invalid Cole state"

translateLedgerStateColeToSophieWrapper ::
     ( SophieBasedEra (SophieEra c)
     , HASH     c ~ Blake2b_256
     , ADDRHASH c ~ Blake2b_224
     )
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       ColeBlock
       (SophieBlock (SophieEra c))
translateLedgerStateColeToSophieWrapper =
    RequireBoth $ \_ (WrapLedgerConfig cfgSophie) ->
    Translate   $ \epochNo ledgerCole ->
      SophieLedgerState {
        sophieLedgerTip =
          translatePointColeToSophie
            (ledgerTipPoint (Proxy @ColeBlock) ledgerCole)
            (coleLedgerTipBlockNo ledgerCole)
      , sophieLedgerState =
          SL.translateToSophieLedgerState
            (sophieLedgerGenesis cfgSophie)
            epochNo
            (coleLedgerState ledgerCole)
      , sophieLedgerTransition =
          SophieTransitionInfo{sophieAfterVoting = 0}
      }

translateChainDepStateColeToSophieWrapper ::
     RequiringBoth
       WrapConsensusConfig
       (Translate WrapChainDepState)
       ColeBlock
       (SophieBlock (SophieEra c))
translateChainDepStateColeToSophieWrapper =
    RequireBoth $ \_ (WrapConsensusConfig cfgSophie) ->
      Translate $ \_ (WrapChainDepState pbftState) ->
        WrapChainDepState $
          translateChainDepStateColeToSophie cfgSophie pbftState

translateChainDepStateColeToSophie ::
     forall bc c.
     ConsensusConfig (TOptimum c)
  -> PBftState bc
  -> TOptimumState c
translateChainDepStateColeToSophie TOptimumConfig { toptimumParams } pbftState =
    -- Note that the 'PBftState' doesn't know about EBBs. So if the last slot of
    -- the Cole era were occupied by an EBB (and no regular block in that same
    -- slot), we would pick the wrong slot here, i.e., the slot of the regular
    -- block before the EBB.
    --
    -- Fortunately, this is impossible for two reasons:
    --
    -- 1. On mainnet we stopped producing EBBs a while before the transition.
    -- 2. The transition happens at the start of an epoch, so if the last slot
    --    were occupied by an EBB, it must have been the EBB at the start of the
    --    previous epoch. This means the previous epoch must have been empty,
    --    which is a violation of the "@k@ blocks per @2k@ slots" property.
    TOptimumState (PBftState.lastSignedSlot pbftState) $
      SL.ChainDepState
        { SL.csProtocol = SL.PrtclState Map.empty nonce nonce
        , SL.csTickn    = SL.TicknState {
              ticknStateEpochNonce    = nonce
            , ticknStatePrevHashNonce = SL.NeutralNonce
            }
          -- Overridden before used
        , SL.csLabNonce = SL.NeutralNonce
        }
  where
    nonce = toptimumInitialNonce toptimumParams

translateLedgerViewColeToSophieWrapper ::
     forall c.
     RequiringBoth
       WrapLedgerConfig
       (TranslateForecast LedgerState WrapLedgerView)
       ColeBlock
       (SophieBlock (SophieEra c))
translateLedgerViewColeToSophieWrapper =
    RequireBoth $ \_ (WrapLedgerConfig cfgSophie) ->
      TranslateForecast (forecast cfgSophie)
  where
    -- We ignore the Cole ledger view and create a new Sophie.
    --
    -- The full Sophie forecast range (stability window) starts from the first
    -- slot of the Sophie era, no matter how many slots there are between the
    -- Cole ledger and the first Sophie slot. Note that this number of slots
    -- is still guaranteed to be less than the forecast range of the HFC in the
    -- Cole era.
    forecast ::
         SophieLedgerConfig (SophieEra c)
      -> Bound
      -> SlotNo
      -> LedgerState ColeBlock
      -> Except
           OutsideForecastRange
           (Ticked (WrapLedgerView (SophieBlock (SophieEra c))))
    forecast cfgSophie bound forecastFor currentColeState
        | forecastFor < maxFor
        = return $
            WrapTickedLedgerView $ TickedOptimumLedgerView $
              SL.mkInitialSophieLedgerView
                (sophieLedgerGenesis cfgSophie)
        | otherwise
        = throwError $ OutsideForecastRange {
              outsideForecastAt     = ledgerTipSlot currentColeState
            , outsideForecastMaxFor = maxFor
            , outsideForecastFor    = forecastFor
            }
      where
        globals = sophieLedgerGlobals cfgSophie
        swindow = SL.stabilityWindow globals

        -- This is the exclusive upper bound of the forecast range
        --
        -- If Sophie's stability window is 0, it means we can't forecast /at
        -- all/ in the Sophie era. Not even to the first slot in the Sophie
        -- era! Remember that forecasting to slot @S@ means forecasting the
        -- ledger view obtained from the ledger state /after/ applying the block
        -- with slot @S@. If the stability window is 0, we can't even forecast
        -- after the very first "virtual" Sophie block, meaning we can't
        -- forecast into the Sophie era when still in the Cole era.
        maxFor :: SlotNo
        maxFor = addSlots swindow (boundSlot bound)

{-------------------------------------------------------------------------------
  Translation from Sophie to Evie
-------------------------------------------------------------------------------}

translateLedgerStateSophieToEvieWrapper ::
     OptimumCrypto c
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       (SophieBlock (SophieEra c))
       (SophieBlock (EvieEra c))
translateLedgerStateSophieToEvieWrapper =
    ignoringBoth $
      Translate $ \_epochNo ->
        unComp . SL.translateEra' () . Comp

translateTxSophieToEvieWrapper ::
     OptimumCrypto c
  => InjectTx
       (SophieBlock (SophieEra c))
       (SophieBlock (EvieEra c))
translateTxSophieToEvieWrapper = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp

translateValidatedTxSophieToEvieWrapper ::
     OptimumCrypto c
  => InjectValidatedTx
       (SophieBlock (SophieEra c))
       (SophieBlock (EvieEra c))
translateValidatedTxSophieToEvieWrapper = InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp

{-------------------------------------------------------------------------------
  Translation from Sophie to Evie
-------------------------------------------------------------------------------}

translateLedgerStateEvieToJenWrapper ::
     OptimumCrypto c
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       (SophieBlock (EvieEra c))
       (SophieBlock (JenEra c))
translateLedgerStateEvieToJenWrapper =
    ignoringBoth $
      Translate $ \_epochNo ->
        unComp . SL.translateEra' () . Comp

{-------------------------------------------------------------------------------
  Translation from Evie to Jen
-------------------------------------------------------------------------------}

translateTxEvieToJenWrapper ::
     OptimumCrypto c
  => InjectTx
       (SophieBlock (EvieEra c))
       (SophieBlock (JenEra c))
translateTxEvieToJenWrapper = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp

translateValidatedTxEvieToJenWrapper ::
     OptimumCrypto c
  => InjectValidatedTx
       (SophieBlock (EvieEra c))
       (SophieBlock (JenEra c))
translateValidatedTxEvieToJenWrapper = InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra () . Comp

{-------------------------------------------------------------------------------
  Translation from Jen to Aurum
-------------------------------------------------------------------------------}

translateLedgerStateJenToAurumWrapper ::
     OptimumCrypto c
  => RequiringBoth
       WrapLedgerConfig
       (Translate LedgerState)
       (SophieBlock (JenEra c))
       (SophieBlock (AurumEra c))
translateLedgerStateJenToAurumWrapper =
    RequireBoth $ \_cfgJen cfgAurum ->
      Translate $ \_epochNo ->
        unComp . SL.translateEra' (getAurumTranslationContext cfgAurum) . Comp

getAurumTranslationContext ::
     WrapLedgerConfig (SophieBlock (AurumEra c))
  -> Aurum.AurumGenesis
getAurumTranslationContext =
    sophieLedgerTranslationContext . unwrapLedgerConfig

translateTxJenToAurumWrapper ::
     OptimumCrypto c
  => Aurum.AurumGenesis
  -> InjectTx
       (SophieBlock (JenEra c))
       (SophieBlock (AurumEra c))
translateTxJenToAurumWrapper ctxt = InjectTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp

translateValidatedTxJenToAurumWrapper ::
     forall c.
     OptimumCrypto c
  => Aurum.AurumGenesis
  -> InjectValidatedTx
       (SophieBlock (JenEra c))
       (SophieBlock (AurumEra c))
translateValidatedTxJenToAurumWrapper ctxt = InjectValidatedTx $
    fmap unComp . eitherToMaybe . runExcept . SL.translateEra ctxt . Comp
