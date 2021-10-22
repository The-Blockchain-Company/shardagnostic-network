{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Shardagnostic.Consensus.Sophie.Ledger.Ledger (
    LedgerState (..)
  , SophieBasedEra
  , SophieLedgerError (..)
  , SophieTip (..)
  , SophieTransition (..)
  , Ticked (..)
  , sophieLedgerTipPoint
  , sophieTipToPoint
    -- * Ledger config
  , SophieLedgerConfig (..)
  , mkSophieLedgerConfig
  , sophieEraParams
  , sophieEraParamsNeverHardForks
  , sophieLedgerGenesis
    -- * Auxiliary
  , SophieLedgerEvent (..)
  , SophieReapplyException (..)
  , getPParams
    -- * Serialisation
  , decodeSophieAnnTip
  , decodeSophieLedgerState
  , encodeSophieAnnTip
  , encodeSophieHeaderState
  , encodeSophieLedgerState
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (decode, encode)
import qualified Control.Exception as Exception
import           Control.Monad.Except
import           Data.Functor ((<&>))
import           Data.Functor.Identity
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Records
import           NoThunks.Class (NoThunks (..))

import           Bcc.Binary (FromCBOR (..), ToCBOR (..), enforceSize)
import           Bcc.Slotting.EpochInfo

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.BlockchainTime.WallClock.Types
import           Shardagnostic.Consensus.Config
import           Shardagnostic.Consensus.Forecast
import           Shardagnostic.Consensus.HardFork.Abstract
import qualified Shardagnostic.Consensus.HardFork.History as HardFork
import           Shardagnostic.Consensus.HardFork.History.Util
import           Shardagnostic.Consensus.HeaderValidation
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.Ledger.CommonProtocolParams
import           Shardagnostic.Consensus.Ledger.Extended
import           Shardagnostic.Consensus.Ledger.SupportsProtocol
import           Shardagnostic.Consensus.Util ((..:))
import           Shardagnostic.Consensus.Util.CBOR (decodeWithOrigin,
                     encodeWithOrigin)
import           Shardagnostic.Consensus.Util.Versioned

import qualified Bcc.Ledger.Core as Core
import qualified Bcc.Ledger.Era as Core
import qualified Control.State.Transition.Extended as STS
import qualified Sophie.Spec.Ledger.API as SL

import qualified Sophie.Spec.Ledger.STS.Chain as SL (PredicateFailure)

import           Shardagnostic.Consensus.Sophie.Eras (EraCrypto)
import           Shardagnostic.Consensus.Sophie.Ledger.Block
import           Shardagnostic.Consensus.Sophie.Ledger.Config
import           Shardagnostic.Consensus.Sophie.Ledger.TOptimum ()
import           Shardagnostic.Consensus.Sophie.Protocol (MaxMajorProtVer (..),
                     Ticked (TickedOptimumLedgerView))
import           Shardagnostic.Consensus.Sophie.Protocol.Util (isNewEpoch)

{-------------------------------------------------------------------------------
  Ledger errors
-------------------------------------------------------------------------------}

newtype SophieLedgerError era = BBodyError (SL.BlockTransitionError era)
  deriving (Generic)

deriving instance SophieBasedEra era => Eq   (SophieLedgerError era)
deriving instance SophieBasedEra era => Show (SophieLedgerError era)

instance SophieBasedEra era => NoThunks (SophieLedgerError era)

{-------------------------------------------------------------------------------
  Config
-------------------------------------------------------------------------------}

data SophieLedgerConfig era = SophieLedgerConfig {
      sophieLedgerCompactGenesis     :: !(CompactGenesis era)
      -- | Derived from 'sophieLedgerGenesis' but we store a cached version
      -- because it used very often.
    , sophieLedgerGlobals            :: !SL.Globals
    , sophieLedgerTranslationContext :: !(Core.TranslationContext era)
    }
  deriving (Generic, NoThunks)

sophieLedgerGenesis :: SophieLedgerConfig era -> SL.SophieGenesis era
sophieLedgerGenesis = getCompactGenesis . sophieLedgerCompactGenesis

sophieEraParams ::
     SL.SophieGenesis era
  -> HardFork.EraParams
sophieEraParams genesis = HardFork.EraParams {
      eraEpochSize  = SL.sgEpochLength genesis
    , eraSlotLength = mkSlotLength $ SL.sgSlotLength genesis
    , eraSafeZone   = HardFork.StandardSafeZone stabilityWindow
    }
  where
    stabilityWindow =
        SL.computeStabilityWindow
          (SL.sgSecurityParam genesis)
          (SL.sgActiveSlotCoeff genesis)

-- | Separate variant of 'sophieEraParams' to be used for a Sophie-only chain.
sophieEraParamsNeverHardForks :: SL.SophieGenesis era -> HardFork.EraParams
sophieEraParamsNeverHardForks genesis = HardFork.EraParams {
      eraEpochSize  = SL.sgEpochLength genesis
    , eraSlotLength = mkSlotLength $ SL.sgSlotLength genesis
    , eraSafeZone   = HardFork.UnsafeIndefiniteSafeZone
    }

mkSophieLedgerConfig
  :: SL.SophieGenesis era
  -> Core.TranslationContext era
  -> EpochInfo (Except HardFork.PastHorizonException)
  -> MaxMajorProtVer
  -> SophieLedgerConfig era
mkSophieLedgerConfig genesis transCtxt epochInfo mmpv =
    SophieLedgerConfig {
        sophieLedgerCompactGenesis     = compactGenesis genesis
      , sophieLedgerGlobals            =
          SL.mkSophieGlobals
            genesis
            (HardFork.toPureEpochInfo epochInfo)
            maxMajorPV
      , sophieLedgerTranslationContext = transCtxt
      }
  where
    MaxMajorProtVer maxMajorPV = mmpv

type instance LedgerCfg (LedgerState (SophieBlock era)) = SophieLedgerConfig era

{-------------------------------------------------------------------------------
  LedgerState
-------------------------------------------------------------------------------}

data SophieTip era = SophieTip {
      sophieTipSlotNo  :: !SlotNo
    , sophieTipBlockNo :: !BlockNo
    , sophieTipHash    :: !(HeaderHash (SophieBlock era))
    }
  deriving (Eq, Show, Generic, NoThunks)

sophieTipToPoint :: WithOrigin (SophieTip era) -> Point (SophieBlock era)
sophieTipToPoint Origin          = GenesisPoint
sophieTipToPoint (NotOrigin tip) = BlockPoint (sophieTipSlotNo tip)
                                               (sophieTipHash   tip)

data instance LedgerState (SophieBlock era) = SophieLedgerState {
      sophieLedgerTip        :: !(WithOrigin (SophieTip era))
    , sophieLedgerState      :: !(SL.NewEpochState era)
    , sophieLedgerTransition :: !SophieTransition
    }
  deriving (Generic)

deriving instance SophieBasedEra era => Show     (LedgerState (SophieBlock era))
deriving instance SophieBasedEra era => Eq       (LedgerState (SophieBlock era))
deriving instance SophieBasedEra era => NoThunks (LedgerState (SophieBlock era))

-- | Information required to determine the hard fork point from Sophie to the
-- next ledger
newtype SophieTransition = SophieTransitionInfo {
      -- | The number of blocks in this epoch past the voting deadline
      --
      -- We record this to make sure that we can tell the HFC about hard forks
      -- if and only if we are certain:
      --
      -- 1. Blocks that came in within an epoch after the 4k/f voting deadline
      --    are not relevant (10k/f - 2 * 3k/f).
      -- 2. Since there are slots between blocks, we are probably only sure that
      --    there will be no more relevant block when we have seen the first
      --    block after the deadline.
      -- 3. If we count how many blocks we have seen post deadline, and we have
      --    reached k of them, we know that that last pre-deadline block won't
      --    be rolled back anymore.
      -- 4. At this point we can look at the ledger state and see which
      --    proposals we accepted in the voting period, if any, and notify the
      --    HFC is one of them indicates a transition.
      sophieAfterVoting :: Word32
    }
  deriving stock   (Eq, Show, Generic)
  deriving newtype (NoThunks)

sophieLedgerTipPoint :: LedgerState (SophieBlock era) -> Point (SophieBlock era)
sophieLedgerTipPoint = sophieTipToPoint . sophieLedgerTip

instance SophieBasedEra era => UpdateLedger (SophieBlock era)

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

instance GetTip (LedgerState (SophieBlock era)) where
  getTip = castPoint . sophieLedgerTipPoint

instance GetTip (Ticked (LedgerState (SophieBlock era))) where
  getTip = castPoint . untickedSophieLedgerTipPoint

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

-- | Ticking only affects the state itself
data instance Ticked (LedgerState (SophieBlock era)) = TickedSophieLedgerState {
      untickedSophieLedgerTip      :: !(WithOrigin (SophieTip era))
      -- | We are counting blocks within an epoch, this means:
      --
      -- 1. We are only incrementing this when /applying/ a block, not when ticking.
      -- 2. However, we count within an epoch, which is slot-based. So the count
      --    must be reset when /ticking/, not when applying a block.
    , tickedSophieLedgerTransition :: !SophieTransition
    , tickedSophieLedgerState      :: !(SL.NewEpochState era)
    }
  deriving (Generic)

deriving instance SophieBasedEra era
               => NoThunks (Ticked (LedgerState (SophieBlock era)))

untickedSophieLedgerTipPoint ::
     Ticked (LedgerState (SophieBlock era))
  -> Point (SophieBlock era)
untickedSophieLedgerTipPoint = sophieTipToPoint . untickedSophieLedgerTip

instance SophieBasedEra era => IsLedger (LedgerState (SophieBlock era)) where
  type LedgerErr (LedgerState (SophieBlock era)) = SophieLedgerError era

  type AuxLedgerEvent (LedgerState (SophieBlock era)) = SophieLedgerEvent era

  applyChainTickLedgerResult cfg slotNo SophieLedgerState{
                                sophieLedgerTip
                              , sophieLedgerState
                              , sophieLedgerTransition
                              } =
      swizzle appTick <&> \l' ->
      TickedSophieLedgerState {
          untickedSophieLedgerTip =
            sophieLedgerTip
        , tickedSophieLedgerTransition =
            -- The voting resets each epoch
            if isNewEpoch ei (sophieTipSlotNo <$> sophieLedgerTip) slotNo then
              SophieTransitionInfo { sophieAfterVoting = 0 }
            else
              sophieLedgerTransition
        , tickedSophieLedgerState = l'
        }
    where
      globals = sophieLedgerGlobals cfg

      ei :: EpochInfo Identity
      ei = SL.epochInfo globals

      swizzle (l, events) =
          LedgerResult {
              lrEvents = map SophieLedgerEventTICK events
            , lrResult = l
            }

      appTick =
        SL.applyTickOpts
          STS.ApplySTSOpts {
              asoAssertions = STS.globalAssertionPolicy
            , asoValidation = STS.ValidateAll
            , asoEvents     = STS.EPReturn
            }
          globals
          sophieLedgerState
          slotNo

-- | All events emitted by the Sophie ledger API
data SophieLedgerEvent era =
    -- | An event emitted when (re)applying a block
    SophieLedgerEventBBODY (STS.Event (Core.EraRule "BBODY" era))
    -- | An event emitted during the chain tick
  | SophieLedgerEventTICK  (STS.Event (Core.EraRule "TICK"  era))

instance SophieBasedEra era
      => ApplyBlock (LedgerState (SophieBlock era)) (SophieBlock era) where
  -- Note: in the Sophie ledger, the @CHAIN@ rule is used to apply a whole
  -- block. In consensus, we split up the application of a block to the ledger
  -- into separate steps that are performed together by 'applyExtLedgerState':
  --
  -- + 'applyChainTickLedgerResult': executes the @TICK@ transition
  -- + 'validateHeader':
  --    - 'validateEnvelope': executes the @chainChecks@
  --    - 'updateChainDepState': executes the @PRTCL@ transition
  -- + 'applyBlockLedgerResult': executes the @BBODY@ transition
  --
  applyBlockLedgerResult =
      applyHelper (swizzle ..: appBlk)
    where
      swizzle m =
        withExcept BBodyError m <&> \(l, events) ->
          LedgerResult {
              lrEvents = map SophieLedgerEventBBODY events
            , lrResult = l
            }

      -- Apply the BBODY transition using the ticked state
      appBlk =
        SL.applyBlockOpts
          STS.ApplySTSOpts {
              asoAssertions = STS.globalAssertionPolicy
            , asoValidation = STS.ValidateAll
            , asoEvents     = STS.EPReturn
            }

  reapplyBlockLedgerResult =
      runIdentity ..: applyHelper (swizzle ..: reappBlk)
    where
      swizzle m = case runExcept m of
        Left err          ->
          Exception.throw $! SophieReapplyException @era err
        Right (l, events) ->
          pure LedgerResult {
              lrEvents = map SophieLedgerEventBBODY events
            , lrResult = l
            }

      -- Reapply the BBODY transition using the ticked state
      reappBlk =
        SL.applyBlockOpts
          STS.ApplySTSOpts {
                  asoAssertions = STS.AssertionsOff
                , asoValidation = STS.ValidateNone
                , asoEvents     = STS.EPReturn
                }

data SophieReapplyException =
  forall era. Show (SL.BlockTransitionError era)
  => SophieReapplyException (SL.BlockTransitionError era)

instance Show SophieReapplyException where
  show (SophieReapplyException err) = "(SophieReapplyException " <> show err <> ")"

instance Exception.Exception SophieReapplyException where

applyHelper ::
     (SophieBasedEra era, Monad m)
  => (   SL.Globals
      -> SL.NewEpochState era
      -> SL.Block era
      -> m (LedgerResult
              (LedgerState (SophieBlock era))
              (SL.NewEpochState era)
           )
     )
  -> LedgerConfig (SophieBlock era)
  -> SophieBlock era
  -> Ticked (LedgerState (SophieBlock era))
  -> m (LedgerResult
          (LedgerState (SophieBlock era))
          (LedgerState (SophieBlock era)))
applyHelper f cfg blk TickedSophieLedgerState{
                          tickedSophieLedgerTransition
                        , tickedSophieLedgerState
                        } = do
    ledgerResult <- f globals tickedSophieLedgerState (sophieBlockRaw blk)

    return $ ledgerResult <&> \newNewEpochState -> SophieLedgerState {
        sophieLedgerTip = NotOrigin SophieTip {
            sophieTipBlockNo = blockNo   blk
          , sophieTipSlotNo  = blockSlot blk
          , sophieTipHash    = blockHash blk
          }
      , sophieLedgerState =
          newNewEpochState
      , sophieLedgerTransition = SophieTransitionInfo {
            sophieAfterVoting =
              -- We count the number of blocks that have been applied after the
              -- voting deadline has passed.
              (if blockSlot blk >= votingDeadline then succ else id) $
                sophieAfterVoting tickedSophieLedgerTransition
          }
      }
  where
    globals = sophieLedgerGlobals cfg
    swindow = SL.stabilityWindow globals

    ei :: EpochInfo Identity
    ei = SL.epochInfo globals

    -- The start of the next epoch is within the safe zone, always.
    startOfNextEpoch :: SlotNo
    startOfNextEpoch = runIdentity $ do
        blockEpoch <- epochInfoEpoch ei (blockSlot blk)
        let nextEpoch = succ blockEpoch
        epochInfoFirst ei nextEpoch

    -- The block must come in strictly before the voting deadline
    -- See Fig 13, "Protocol Parameter Update Inference Rules", of the
    -- Sophie specification.
    votingDeadline :: SlotNo
    votingDeadline = subSlots (2 * swindow) startOfNextEpoch

instance SophieBasedEra era
      => LedgerSupportsProtocol (SophieBlock era) where
  protocolLedgerView _cfg = TickedOptimumLedgerView
                          . SL.currentLedgerView
                          . tickedSophieLedgerState

  ledgerViewForecastAt cfg ledgerState = Forecast at $ \for -> if
      | NotOrigin for == at ->
        return $ TickedOptimumLedgerView $ SL.currentLedgerView sophieLedgerState
      | for < maxFor ->
        return $ futureLedgerView for
      | otherwise ->
        throwError $ OutsideForecastRange {
            outsideForecastAt     = at
          , outsideForecastMaxFor = maxFor
          , outsideForecastFor    = for
          }
    where
      SophieLedgerState { sophieLedgerState } = ledgerState
      globals = sophieLedgerGlobals cfg
      swindow = SL.stabilityWindow globals
      at      = ledgerTipSlot ledgerState

      -- | 'SL.futureLedgerView' imposes its own bounds. Those bounds could
      -- /exceed/ the 'maxFor' we have computed, but should never be /less/.
      futureLedgerView :: SlotNo -> Ticked (SL.LedgerView (EraCrypto era))
      futureLedgerView =
            either
              (\e -> error ("futureLedgerView failed: " <> show e))
              TickedOptimumLedgerView
          . SL.futureLedgerView globals sophieLedgerState

      -- Exclusive upper bound
      maxFor :: SlotNo
      maxFor = addSlots swindow $ succWithOrigin at

instance HasHardForkHistory (SophieBlock era) where
  type HardForkIndices (SophieBlock era) = '[SophieBlock era]
  hardForkSummary = neverForksHardForkSummary $
      sophieEraParamsNeverHardForks . sophieLedgerGenesis

instance SophieBasedEra era
      => CommonProtocolParams (SophieBlock era) where
  maxHeaderSize = fromIntegral . getField @"_maxBHSize" . getPParams . sophieLedgerState
  maxTxSize     = fromIntegral . getField @"_maxTxSize" . getPParams . sophieLedgerState

{-------------------------------------------------------------------------------
  ValidateEnvelope
-------------------------------------------------------------------------------}

instance SophieBasedEra era => BasicEnvelopeValidation (SophieBlock era) where
  -- defaults all OK

instance SophieBasedEra era => ValidateEnvelope (SophieBlock era) where
  type OtherHeaderEnvelopeError (SophieBlock era) =
    SL.PredicateFailure (SL.CHAIN era)

  additionalEnvelopeChecks cfg (TickedOptimumLedgerView ledgerView) hdr =
      SL.chainChecks globals (SL.lvChainChecks ledgerView) (sophieHeaderRaw hdr)
    where
      globals = sophieLedgerGlobals (configLedger cfg)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

getPParams :: SL.NewEpochState era -> Core.PParams era
getPParams = SL.esPp . SL.nesEs

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

-- | Current version
--
-- o 'serialisationFormatVersion0' used to include the 'LedgerViewHistory', but
--   since we had to break binary backwards compatibility of the 'TOptimumState',
--   we dropped backwards compatibility with 'serialisationFormatVersion0' too.
-- o 'serialisationFormatVersion1' did not include a 'BlockNo' at the tip of
--   the ledger, which was introduced in version 2. Again, since we broke
--   compat anyway, we dropped support for version 1.
serialisationFormatVersion2 :: VersionNumber
serialisationFormatVersion2 = 2

encodeSophieAnnTip ::
     SophieBasedEra era
  => AnnTip (SophieBlock era) -> Encoding
encodeSophieAnnTip = defaultEncodeAnnTip toCBOR

decodeSophieAnnTip ::
     SophieBasedEra era
  => Decoder s (AnnTip (SophieBlock era))
decodeSophieAnnTip = defaultDecodeAnnTip fromCBOR

encodeSophieHeaderState ::
     SophieBasedEra era
  => HeaderState (SophieBlock era)
  -> Encoding
encodeSophieHeaderState = encodeHeaderState
    encode
    encodeSophieAnnTip

encodeSophieTip :: SophieBasedEra era => SophieTip era -> Encoding
encodeSophieTip SophieTip {
                     sophieTipSlotNo
                   , sophieTipBlockNo
                   , sophieTipHash
                   } = mconcat [
      CBOR.encodeListLen 3
    , encode sophieTipSlotNo
    , encode sophieTipBlockNo
    , encode sophieTipHash
    ]

decodeSophieTip :: SophieBasedEra era => Decoder s (SophieTip era)
decodeSophieTip = do
    enforceSize "SophieTip" 3
    sophieTipSlotNo  <- decode
    sophieTipBlockNo <- decode
    sophieTipHash    <- decode
    return SophieTip {
        sophieTipSlotNo
      , sophieTipBlockNo
      , sophieTipHash
      }

encodeSophieTransition :: SophieTransition -> Encoding
encodeSophieTransition SophieTransitionInfo{sophieAfterVoting} = mconcat [
      CBOR.encodeWord32 sophieAfterVoting
    ]

decodeSophieTransition :: Decoder s SophieTransition
decodeSophieTransition = do
    sophieAfterVoting <- CBOR.decodeWord32
    return SophieTransitionInfo{sophieAfterVoting}

encodeSophieLedgerState ::
     SophieBasedEra era
  => LedgerState (SophieBlock era)
  -> Encoding
encodeSophieLedgerState
    SophieLedgerState { sophieLedgerTip
                       , sophieLedgerState
                       , sophieLedgerTransition
                       } =
    encodeVersion serialisationFormatVersion2 $ mconcat [
        CBOR.encodeListLen 3
      , encodeWithOrigin encodeSophieTip sophieLedgerTip
      , toCBOR sophieLedgerState
      , encodeSophieTransition sophieLedgerTransition
      ]

decodeSophieLedgerState ::
     forall era s. SophieBasedEra era
  => Decoder s (LedgerState (SophieBlock era))
decodeSophieLedgerState = decodeVersion [
      (serialisationFormatVersion2, Decode decodeSophieLedgerState2)
    ]
  where
    decodeSophieLedgerState2 :: Decoder s' (LedgerState (SophieBlock era))
    decodeSophieLedgerState2 = do
      enforceSize "LedgerState SophieBlock" 3
      sophieLedgerTip        <- decodeWithOrigin decodeSophieTip
      sophieLedgerState      <- fromCBOR
      sophieLedgerTransition <- decodeSophieTransition
      return SophieLedgerState {
          sophieLedgerTip
        , sophieLedgerState
        , sophieLedgerTransition
        }
