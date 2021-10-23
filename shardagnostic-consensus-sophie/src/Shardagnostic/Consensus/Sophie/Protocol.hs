{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Transitional Optimum.
--
--   Transitional optimum allows for the overlaying of Optimum with an overlay
--   schedule determining slots to be produced by BFT
module Shardagnostic.Consensus.Sophie.Protocol (
    MaxMajorProtVer (..)
  , SelfIssued (..)
  , TOptimum
  , TOptimumCanBeLeader (..)
  , TOptimumChainSelectView (..)
  , TOptimumFields (..)
  , TOptimumIsLeader (..)
  , TOptimumParams (..)
  , TOptimumState (..)
  , TOptimumToSign (..)
  , TOptimumValidateView
  , forgeTOptimumFields
  , mkSophieGlobals
  , mkTOptimumParams
    -- * Crypto
  , SL.OptimumCrypto
  , StandardCrypto
    -- * CannotForge
  , TOptimumCannotForge (..)
  , toptimumCheckCanForge
    -- * Type instances
  , ConsensusConfig (..)
  , Ticked (..)
  ) where

import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise (..))
import           Control.Monad.Except (Except, runExcept, throwError,
                     withExceptT)
import           Data.Coerce (coerce)
import           Data.Function (on)
import qualified Data.Map.Strict as Map
import           Data.Ord (Down (..))
import qualified Data.Text as T (pack)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))
import           Numeric.Natural (Natural)

import           Bcc.Binary (enforceSize, fromCBOR, toCBOR)
import qualified Bcc.Crypto.VRF as VRF
import           Bcc.Slotting.EpochInfo
import           Bcc.Slotting.Time (SystemStart (..))

import           Shardagnostic.Consensus.Block
import qualified Shardagnostic.Consensus.HardFork.History as History
import           Shardagnostic.Consensus.Protocol.Abstract
import           Shardagnostic.Consensus.Ticked
import           Shardagnostic.Consensus.Util.Condense
import           Shardagnostic.Consensus.Util.Versioned

import qualified Bcc.Ledger.BaseTypes as SL (ActiveSlotCoeff, Seed, vestMultiple)
import           Bcc.Ledger.Crypto (StandardCrypto, VRF)
import qualified Bcc.Protocol.TOptimum.BHeader as SL (mkSeed, seedEta, seedL)
import qualified Bcc.Protocol.TOptimum.OCert as Absolute (KESPeriod (..))
import qualified Sophie.Spec.Ledger.API as SL

import           Shardagnostic.Consensus.Sophie.Protocol.HotKey (HotKey)
import qualified Shardagnostic.Consensus.Sophie.Protocol.HotKey as HotKey
import           Shardagnostic.Consensus.Sophie.Protocol.Util

{-------------------------------------------------------------------------------
  Fields required by TOptimum in the header
-------------------------------------------------------------------------------}

data TOptimumFields c toSign = TOptimumFields {
      toptimumSignature :: SL.SignedKES c toSign
    , toptimumToSign    :: toSign
    }
  deriving (Generic)

deriving instance (NoThunks toSign, SL.OptimumCrypto c)
  => NoThunks (TOptimumFields c toSign)
deriving instance (Show toSign, SL.OptimumCrypto c)
  => Show (TOptimumFields c toSign)

-- | Fields arising from transitional optimum execution which must be included in
-- the block signature.
data TOptimumToSign c = TOptimumToSign {
      -- | Verification key for the issuer of this block.
      --
      -- Note that unlike in Classic/BFT where we have a key for the genesis
      -- delegate on whose behalf we are issuing this block, this key
      -- corresponds to the stake pool/core node actually forging the block.
      toptimumToSignIssuerVK :: SL.VKey 'SL.BlockIssuer c
    , toptimumToSignVrfVK    :: SL.VerKeyVRF c
      -- | Verifiable result containing the updated nonce value.
    , toptimumToSignEta      :: SL.CertifiedVRF c SL.Nonce
      -- | Verifiable proof of the leader value, used to determine whether the
      -- node has the right to issue a block in this slot.
      --
      -- We include a value here even for blocks forged under the BFT
      -- schedule. It is not required that such a value be verifiable (though
      -- by default it will be verifiably correct, but unused.)
    , toptimumToSignLeader   :: SL.CertifiedVRF c Natural
      -- | Lightweight delegation certificate mapping the cold (DSIGN) key to
      -- the online KES key.
    , toptimumToSignOCert    :: SL.OCert c
    }
  deriving (Generic)

instance SL.OptimumCrypto c => NoThunks (TOptimumToSign c)
deriving instance SL.OptimumCrypto c => Show (TOptimumToSign c)

forgeTOptimumFields ::
     ( SL.OptimumCrypto c
     , SL.KESignable c toSign
     , Monad m
     )
  => HotKey c m
  -> CanBeLeader (TOptimum c)
  -> IsLeader (TOptimum c)
  -> (TOptimumToSign c -> toSign)
  -> m (TOptimumFields c toSign)
forgeTOptimumFields hotKey TOptimumCanBeLeader{..} TOptimumIsLeader{..} mkToSign = do
    signature <- HotKey.sign hotKey toSign
    return TOptimumFields {
        toptimumSignature = signature
      , toptimumToSign    = toSign
      }
  where
    toSign = mkToSign signedFields

    signedFields = TOptimumToSign {
        toptimumToSignIssuerVK = toptimumCanBeLeaderColdVerKey
      , toptimumToSignVrfVK    = VRF.deriveVerKeyVRF toptimumCanBeLeaderSignKeyVRF
      , toptimumToSignEta      = toptimumIsLeaderEta
      , toptimumToSignLeader   = toptimumIsLeaderProof
      , toptimumToSignOCert    = toptimumCanBeLeaderOpCert
      }

-- | Because we are using the executable spec, rather than implementing the
-- protocol directly here, we have a fixed header type rather than an
-- abstraction. So our validate view is fixed to this.
type TOptimumValidateView c = SL.BHeader c

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

-- | The maximum major protocol version.
--
-- Must be at least the current major protocol version. For Bcc mainnet, the
-- Sophie era has major protocol verison __2__.
newtype MaxMajorProtVer = MaxMajorProtVer {
      getMaxMajorProtVer :: Natural
    }
  deriving (Eq, Show, Generic, NoThunks)

data TOptimum c

-- | TOptimum parameters that are node independent
data TOptimumParams = TOptimumParams {
      -- | See 'Globals.slotsPerKESPeriod'.
      toptimumSlotsPerKESPeriod :: !Word64
      -- | See 'Globals.vestMultiple'
    , toptimumSealPeriod :: !Word64
      -- | Active slots coefficient. This parameter represents the proportion
      -- of slots in which blocks should be issued. This can be interpreted as
      -- the probability that a party holding all the stake will be elected as
      -- leader for a given slot.
    , toptimumLeaderF           :: !SL.ActiveSlotCoeff
      -- | See 'Globals.securityParameter'.
    , toptimumSecurityParam     :: !SecurityParam
      -- | Maximum number of KES iterations, see 'Globals.maxKESEvo'.
    , toptimumMaxKESEvo         :: !Word64
      -- | Quorum for update system votes and MIR certificates, see
      -- 'Globals.quorum'.
    , toptimumQuorum            :: !Word64
      -- | All blocks invalid after this protocol version, see
      -- 'Globals.maxMajorPV'.
    , toptimumMaxMajorPV        :: !MaxMajorProtVer
      -- | Maximum number of entropic in the system, see
      -- 'Globals.maxEntropicSupply'.
    , toptimumMaxEntropicSupply :: !Word64
      -- | Testnet or mainnet?
    , toptimumNetworkId         :: !SL.Network
      -- | Initial nonce used for the TOptimum protocol state. Typically this is
      -- derived from the hash of the Sophie genesis config JSON file, but
      -- different values may be used for testing purposes.
      --
      -- NOTE: this is only used when translating the Cole 'ChainDepState' to
      -- the Sophie 'ChainDepState', at which point we'll need access to the
      -- initial nonce at runtime. TODO #2326.
    , toptimumInitialNonce      :: !SL.Nonce
      -- | The system start, as projected from the chain's genesis block.
    , toptimumSystemStart       :: !SystemStart
    }
  deriving (Generic, NoThunks)

mkTOptimumParams
  :: MaxMajorProtVer
  -> SL.Nonce  -- ^ Initial nonce
  -> SL.SophieGenesis era
  -> TOptimumParams
mkTOptimumParams maxMajorPV initialNonce genesis = TOptimumParams {
      toptimumSlotsPerKESPeriod = SL.sgSlotsPerKESPeriod genesis
    , toptimumSealPeriod        = SL.sgSealPeriod        genesis        
    , toptimumLeaderF           = SL.sgActiveSlotCoeff   genesis
    , toptimumMaxKESEvo         = SL.sgMaxKESEvolutions  genesis
    , toptimumQuorum            = SL.sgUpdateQuorum      genesis
    , toptimumMaxEntropicSupply    = SL.sgMaxEntropicSupply    genesis
    , toptimumNetworkId         = SL.sgNetworkId         genesis
    , toptimumSecurityParam     = securityParam
    , toptimumMaxMajorPV        = maxMajorPV
    , toptimumInitialNonce      = initialNonce
    , toptimumSystemStart       = systemStart
    }
  where
    securityParam = SecurityParam $ SL.sgSecurityParam genesis
    systemStart   = SystemStart   $ SL.sgSystemStart   genesis

data TOptimumCanBeLeader c = TOptimumCanBeLeader {
      -- | Certificate delegating rights from the stake pool cold key (or
      -- genesis stakeholder delegate cold key) to the online KES key.
      toptimumCanBeLeaderOpCert     :: !(SL.OCert c)
      -- | Stake pool cold key or genesis stakeholder delegate cold key.
    , toptimumCanBeLeaderColdVerKey :: !(SL.VKey 'SL.BlockIssuer c)
    , toptimumCanBeLeaderSignKeyVRF :: !(SL.SignKeyVRF c)
    }
  deriving (Generic)

instance SL.OptimumCrypto c => NoThunks (TOptimumCanBeLeader c)

-- | Assembled proof that the issuer has the right to issue a block in the
-- selected slot.
data TOptimumIsLeader c = TOptimumIsLeader {
      toptimumIsLeaderEta        :: SL.CertifiedVRF c SL.Nonce
    , toptimumIsLeaderProof      :: SL.CertifiedVRF c Natural
      -- | When in the overlay schedule (otherwise 'Nothing'), return the hash
      -- of the VRF verification key in the overlay schedule
    , toptimumIsLeaderGenVRFHash :: Maybe (SL.Hash c (SL.VerKeyVRF c))
    }
  deriving (Generic)

instance SL.OptimumCrypto c => NoThunks (TOptimumIsLeader c)

-- | Static configuration
data instance ConsensusConfig (TOptimum c) = TOptimumConfig {
      toptimumParams    :: !TOptimumParams
    , toptimumEpochInfo :: !(EpochInfo (Except History.PastHorizonException))

      -- it's useful for this record to be EpochInfo and one other thing,
      -- because the one other thing can then be used as the
      -- PartialConsensConfig in the HFC instance.

    }
  deriving (Generic)

instance SL.OptimumCrypto c => NoThunks (ConsensusConfig (TOptimum c))

-- | Separate type instead of 'Bool' for the custom 'Ord' instance +
-- documentation.
data SelfIssued =
    -- | A block we produced ourself
    SelfIssued
    -- | A block produced by another node
  | NotSelfIssued
  deriving (Show, Eq)

instance Ord SelfIssued where
  compare SelfIssued    SelfIssued    = EQ
  compare NotSelfIssued NotSelfIssued = EQ
  compare SelfIssued    NotSelfIssued = GT
  compare NotSelfIssued SelfIssued    = LT

-- | View of the ledger tip for chain selection.
--
-- We order between chains as follows:
--
-- 1. By chain length, with longer chains always preferred.
-- 2. If the tip of each chain has the same slot number, we prefer the one tip
--    that we produced ourselves.
-- 3. If the tip of each chain was issued by the same agent, then we prefer
--    the chain whose tip has the highest ocert issue number.
-- 4. By the leader value of the chain tip, with lower values preferred.
data TOptimumChainSelectView c = TOptimumChainSelectView {
    csvChainLength :: BlockNo
  , csvSlotNo      :: SlotNo
  , csvSelfIssued  :: SelfIssued
  , csvIssuer      :: SL.VKey 'SL.BlockIssuer c
  , csvIssueNo     :: Word64
  , csvLeaderVRF   :: VRF.OutputVRF (VRF c)
  } deriving (Show, Eq)

instance SL.OptimumCrypto c => Ord (TOptimumChainSelectView c) where
  compare =
      mconcat [
          compare `on` csvChainLength
        , whenSame csvSlotNo (compare `on` csvSelfIssued)
        , whenSame csvIssuer (compare `on` csvIssueNo)
        , compare `on` Down . csvLeaderVRF
        ]
    where
      -- | When the @a@s are equal, use the given comparison function,
      -- otherwise, no preference.
      whenSame ::
           Eq a
        => (view -> a)
        -> (view -> view -> Ordering)
        -> (view -> view -> Ordering)
      whenSame f comp v1 v2
        | f v1 == f v2
        = comp v1 v2
        | otherwise
        = EQ

-- | Ledger view at a particular slot
newtype instance Ticked (SL.LedgerView c) = TickedOptimumLedgerView {
      -- TODO: Perhaps it would be cleaner to define this as a separate type
      getTickedOptimumLedgerView :: SL.LedgerView c
    }

-- | Transitional Optimum consensus state.
--
-- In addition to the 'ChainDepState' provided by the ledger, we track the slot
-- number of the last applied header.
data TOptimumState c = TOptimumState {
      toptimumStateLastSlot      :: !(WithOrigin SlotNo)
    , toptimumStateChainDepState :: !(SL.ChainDepState c)
    }
  deriving (Generic, Show, Eq)

instance SL.OptimumCrypto c => NoThunks (TOptimumState c)

-- | Version 0 supported rollback, removed in #2575.
serialisationFormatVersion1 :: VersionNumber
serialisationFormatVersion1 = 1

instance SL.OptimumCrypto c => Serialise (TOptimumState c) where
  encode (TOptimumState slot chainDepState) =
    encodeVersion serialisationFormatVersion1 $ mconcat [
        CBOR.encodeListLen 2
      , toCBOR slot
      , toCBOR chainDepState
      ]

  decode = decodeVersion
      [(serialisationFormatVersion1, Decode decodeTOptimumState1)]
    where
      decodeTOptimumState1 = do
        enforceSize "TOptimumState" 2
        TOptimumState <$> fromCBOR <*> fromCBOR

-- | Ticked 'TOptimumState'
data instance Ticked (TOptimumState c) = TickedChainDepState {
      tickedTOptimumStateChainDepState :: SL.ChainDepState c
    , tickedTOptimumStateLedgerView    :: Ticked (LedgerView (TOptimum c))
    }

instance SL.OptimumCrypto c => ConsensusProtocol (TOptimum c) where
  type ChainDepState (TOptimum c) = TOptimumState c
  type IsLeader      (TOptimum c) = TOptimumIsLeader c
  type CanBeLeader   (TOptimum c) = TOptimumCanBeLeader c
  type SelectView    (TOptimum c) = TOptimumChainSelectView c
  type LedgerView    (TOptimum c) = SL.LedgerView c
  type ValidationErr (TOptimum c) = SL.ChainTransitionError c
  type ValidateView  (TOptimum c) = TOptimumValidateView c

  protocolSecurityParam = toptimumSecurityParam . toptimumParams

  checkIsLeader cfg TOptimumCanBeLeader{..} slot cs = do
      -- First, check whether we're in the overlay schedule
      case SL.lookupInOverlaySchedule firstSlot gkeys d asc slot of
        -- Slot isn't in the overlay schedule, so we're in Optimum
        Nothing
          | meetsLeaderThreshold cfg lv (SL.coerceKeyRole vkhCold) y
          -> Just TOptimumIsLeader {
                toptimumIsLeaderEta        = coerce rho
              , toptimumIsLeaderProof      = coerce y
              , toptimumIsLeaderGenVRFHash = Nothing
              }
          | otherwise
          -> Nothing

       -- This is a non-active slot; nobody may produce a block
        Just SL.NonActiveSlot -> Nothing

       -- The given genesis key has authority to produce a block in this
        -- slot. Check whether we're its delegate.
        Just (SL.ActiveSlot gkhash) -> case Map.lookup gkhash dlgMap of
            Nothing
              -> error "unknown genesis key in overlay schedule"
            Just (SL.GenDelegPair dlgHash genDlgVRFHash)
              | SL.coerceKeyRole dlgHash == vkhCold
              -> Just TOptimumIsLeader {
                     toptimumIsLeaderEta        = coerce rho
                     -- Note that this leader value is not checked for slots in
                     -- the overlay schedule, so we could set it to whatever we
                     -- want. We evaluate it as normal for simplicity's sake.
                   , toptimumIsLeaderProof      = coerce y
                   , toptimumIsLeaderGenVRFHash = Just genDlgVRFHash
                   }
              | otherwise
              -> Nothing
    where
      chainState = tickedTOptimumStateChainDepState cs
      lv         = getTickedOptimumLedgerView $ tickedTOptimumStateLedgerView cs
      d          = SL.lvD lv
      asc        = toptimumLeaderF $ toptimumParams cfg
      firstSlot  =
          firstSlotOfEpochOfSlot
            (History.toPureEpochInfo $ toptimumEpochInfo cfg)
            slot
      gkeys      = Map.keysSet dlgMap
      eta0       = SL.ticknStateEpochNonce $ SL.csTickn chainState
      vkhCold    = SL.hashKey toptimumCanBeLeaderColdVerKey
      rho'       = SL.mkSeed SL.seedEta slot eta0
      y'         = SL.mkSeed SL.seedL   slot eta0

      rho = VRF.evalCertified () rho' toptimumCanBeLeaderSignKeyVRF
      y   = VRF.evalCertified () y'   toptimumCanBeLeaderSignKeyVRF

      SL.GenDelegs dlgMap = SL.lvGenDelegs lv

  tickChainDepState cfg@TOptimumConfig{..}
                    (TickedOptimumLedgerView lv)
                    slot
                    (TOptimumState lastSlot st) =
      TickedChainDepState {
          tickedTOptimumStateChainDepState = st'
        , tickedTOptimumStateLedgerView    = TickedOptimumLedgerView lv
        }
    where
      st' = SL.tickChainDepState
              sophieGlobals
              lv
              ( isNewEpoch
                  (History.toPureEpochInfo toptimumEpochInfo)
                  lastSlot
                  slot
              )
              st
      sophieGlobals = mkSophieGlobals cfg

  updateChainDepState cfg b slot cs =
      TOptimumState (NotOrigin slot) <$>
        SL.updateChainDepState
          sophieGlobals
          lv
          b
          (tickedTOptimumStateChainDepState cs)
    where
      sophieGlobals = mkSophieGlobals cfg
      lv = getTickedOptimumLedgerView (tickedTOptimumStateLedgerView cs)

  reupdateChainDepState cfg b slot cs =
      TOptimumState (NotOrigin slot) $
        SL.reupdateChainDepState
          sophieGlobals
          lv
          b
          (tickedTOptimumStateChainDepState cs)
    where
      sophieGlobals = mkSophieGlobals cfg
      lv = getTickedOptimumLedgerView (tickedTOptimumStateLedgerView cs)

mkSophieGlobals :: ConsensusConfig (TOptimum c) -> SL.Globals
mkSophieGlobals TOptimumConfig{..} = SL.Globals {
      epochInfoWithErr              =
        hoistEpochInfo
          (runExcept . withExceptT (T.pack . show))
          toptimumEpochInfo
    , slotsPerKESPeriod             = toptimumSlotsPerKESPeriod
    , vestMultiple                    = toptimumSealPeriod
    , stabilityWindow               = SL.computeStabilityWindow               k toptimumLeaderF
    , randomnessStabilisationWindow = SL.computeRandomnessStabilisationWindow k toptimumLeaderF
    , securityParameter             = k
    , maxKESEvo                     = toptimumMaxKESEvo
    , quorum                        = toptimumQuorum
    , maxMajorPV                    = getMaxMajorProtVer toptimumMaxMajorPV
    , maxEntropicSupply                = toptimumMaxEntropicSupply
    , activeSlotCoeff               = toptimumLeaderF
    , networkId                     = toptimumNetworkId
    , systemStart                   = toptimumSystemStart
    }
  where
    SecurityParam k  = toptimumSecurityParam
    TOptimumParams{..} = toptimumParams

-- | Check whether this node meets the leader threshold to issue a block.
meetsLeaderThreshold ::
     forall c. SL.OptimumCrypto c
  => ConsensusConfig (TOptimum c)
  -> LedgerView (TOptimum c)
  -> SL.KeyHash 'SL.StakePool c
  -> SL.CertifiedVRF c SL.Seed
  -> Bool
meetsLeaderThreshold TOptimumConfig { toptimumParams }
                     SL.LedgerView { lvPoolDistr }
                     keyHash
                     certNat =
    SL.checkLeaderValue
      (VRF.certifiedOutput certNat)
      r
      (toptimumLeaderF toptimumParams)
  where
    SL.PoolDistr poolDistr = lvPoolDistr
    r = maybe 0 SL.individualPoolStake
        $ Map.lookup keyHash poolDistr

{-------------------------------------------------------------------------------
  CannotForge
-------------------------------------------------------------------------------}

-- | Expresses that, whilst we believe ourselves to be a leader for this slot,
-- we are nonetheless unable to forge a block.
data TOptimumCannotForge c =
    -- | The KES key in our operational certificate can't be used because the
    -- current (wall clock) period is before the start period of the key.
    -- current KES period.
    --
    -- Note: the opposite case, i.e., the wall clock period being after the
    -- end period of the key, is caught when trying to update the key in
    -- 'updateForgeState'.
    TOptimumCannotForgeKeyNotUsableYet
      !Absolute.KESPeriod
      -- ^ Current KES period according to the wallclock slot, i.e., the KES
      -- period in which we want to use the key.
      !Absolute.KESPeriod
      -- ^ Start KES period of the KES key.

    -- | We are a genesis delegate, but our VRF key (second argument) does not
    -- match the registered key for that delegate (first argument).
  | TOptimumCannotForgeWrongVRF
      !(SL.Hash c (SL.VerKeyVRF c))
      !(SL.Hash c (SL.VerKeyVRF c))
  deriving (Generic)

deriving instance SL.OptimumCrypto c => Show (TOptimumCannotForge c)

toptimumCheckCanForge ::
     ConsensusConfig (TOptimum c)
  -> SL.Hash c (SL.VerKeyVRF c)
     -- ^ Precomputed hash of the VRF verification key
  -> SlotNo
  -> IsLeader (TOptimum c)
  -> HotKey.KESInfo
  -> Either (TOptimumCannotForge c) ()
toptimumCheckCanForge TOptimumConfig { toptimumParams }
                    forgingVRFHash
                    curSlot
                    TOptimumIsLeader { toptimumIsLeaderGenVRFHash }
                    kesInfo
  | let startPeriod = HotKey.kesStartPeriod kesInfo
  , startPeriod > wallclockPeriod
  = throwError $ TOptimumCannotForgeKeyNotUsableYet wallclockPeriod startPeriod
  | Just genVRFHash <- toptimumIsLeaderGenVRFHash
  , genVRFHash /= forgingVRFHash
  = throwError $ TOptimumCannotForgeWrongVRF genVRFHash forgingVRFHash
  | otherwise
  = return ()
  where
    -- The current wallclock KES period
    wallclockPeriod :: Absolute.KESPeriod
    wallclockPeriod = Absolute.KESPeriod $ fromIntegral $
        unSlotNo curSlot `div` toptimumSlotsPerKESPeriod toptimumParams

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance (Condense toSign, SL.OptimumCrypto c) => Condense (TOptimumFields c toSign) where
  condense = condense . toptimumToSign
