{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Instances requires for consensus/ledger integration
module Shardagnostic.Consensus.Cole.Ledger.Ledger (
    ColeTransition (..)
    -- * Ledger integration
  , coleEraParams
  , coleEraParamsNeverHardForks
  , initColeLedgerState
    -- * Serialisation
  , decodeColeAnnTip
  , decodeColeLedgerState
  , decodeColeQuery
  , decodeColeResult
  , encodeColeAnnTip
  , encodeColeExtLedgerState
  , encodeColeHeaderState
  , encodeColeLedgerState
  , encodeColeQuery
  , encodeColeResult
    -- * Type family instances
  , BlockQuery (..)
  , LedgerState (..)
  , Ticked (..)
    -- * Auxiliary
  , validationErrorImpossible
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (decode, encode)
import           Control.Monad.Except
import           Data.ByteString (ByteString)
import           Data.Kind (Type)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Bcc.Binary (encodeListLen, enforceSize, fromCBOR, toCBOR)

import qualified Bcc.Chain.Block as CC
import qualified Bcc.Chain.Cole.API as CC
import qualified Bcc.Chain.Genesis as Gen
import qualified Bcc.Chain.UTxO as CC
import qualified Bcc.Chain.Update as Update
import qualified Bcc.Chain.Update.Validation.Endorsement as UPE
import qualified Bcc.Chain.Update.Validation.Interface as UPI
import qualified Bcc.Chain.ValidationMode as CC

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Config
import           Shardagnostic.Consensus.Forecast
import           Shardagnostic.Consensus.HardFork.Abstract
import qualified Shardagnostic.Consensus.HardFork.History as HardFork
import           Shardagnostic.Consensus.HeaderValidation
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.Ledger.CommonProtocolParams
import           Shardagnostic.Consensus.Ledger.Extended
import           Shardagnostic.Consensus.Ledger.Query
import           Shardagnostic.Consensus.Ledger.SupportsPeerSelection
import           Shardagnostic.Consensus.Ledger.SupportsProtocol
import           Shardagnostic.Consensus.Protocol.PBFT
import           Shardagnostic.Consensus.Util (ShowProxy (..), (..:))

import           Shardagnostic.Consensus.Cole.Ledger.Block
import           Shardagnostic.Consensus.Cole.Ledger.Conversions
import           Shardagnostic.Consensus.Cole.Ledger.HeaderValidation ()
import           Shardagnostic.Consensus.Cole.Ledger.PBFT
import           Shardagnostic.Consensus.Cole.Ledger.Serialisation

{-------------------------------------------------------------------------------
  LedgerState
-------------------------------------------------------------------------------}

data instance LedgerState ColeBlock = ColeLedgerState {
      coleLedgerTipBlockNo :: !(WithOrigin BlockNo)
    , coleLedgerState      :: !CC.ChainValidationState
    , coleLedgerTransition :: !ColeTransition
    }
  deriving (Eq, Show, Generic, NoThunks)

-- | Information required to determine the transition from Cole to Sophie
data ColeTransition =
    -- | Per candidate proposal, the 'BlockNo' in which it became a candidate
    --
    -- The HFC needs to know when a candidate proposal becomes stable. We cannot
    -- reliably do this using 'SlotNo': doing so would mean that if we were to
    -- switch to a denser fork, something that was previously deemed stable is
    -- suddenly not deemed stable anymore (although in actuality it still is).
    -- We therefore must do this based on 'BlockNo' instead, but unfortunately
    -- the Cole ledger does not record this information. Therefore, we record
    -- it here instead.
    --
    -- Invariant: the domain of this map should equal the set of candidate
    -- proposals.
    ColeTransitionInfo !(Map Update.ProtocolVersion BlockNo)
  deriving (Eq, Show, Generic, NoThunks)

instance UpdateLedger ColeBlock

type instance LedgerCfg (LedgerState ColeBlock) = Gen.Config

initColeLedgerState :: Gen.Config
                     -> Maybe CC.UTxO -- ^ Optionally override UTxO
                     -> LedgerState ColeBlock
initColeLedgerState genesis mUtxo = ColeLedgerState {
      coleLedgerState      = override mUtxo initState
    , coleLedgerTipBlockNo = Origin
    , coleLedgerTransition = ColeTransitionInfo Map.empty
    }
  where
    initState :: CC.ChainValidationState
    initState = case runExcept $ CC.initialChainValidationState genesis of
      Right st -> st
      Left e   -> error $
        "could not create initial ChainValidationState: " <> show e

    override :: Maybe CC.UTxO
             -> CC.ChainValidationState -> CC.ChainValidationState
    override Nothing     st = st
    override (Just utxo) st = st { CC.cvsUtxo = utxo }

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

instance GetTip (LedgerState ColeBlock) where
  getTip = castPoint . getColeTip . coleLedgerState

instance GetTip (Ticked (LedgerState ColeBlock)) where
  getTip = castPoint . getColeTip . tickedColeLedgerState

getColeTip :: CC.ChainValidationState -> Point ColeBlock
getColeTip state =
    case CC.cvsPreviousHash state of
      -- In this case there are no blocks in the ledger state. The genesis
      -- block does not occupy a slot, so its point is Origin.
      Left _genHash -> GenesisPoint
      Right hdrHash -> BlockPoint slot (ColeHash hdrHash)
        where
          slot = fromColeSlotNo (CC.cvsLastSlot state)

{-------------------------------------------------------------------------------
  Ticked ledger state
-------------------------------------------------------------------------------}

-- | The ticked Cole ledger state
data instance Ticked (LedgerState ColeBlock) = TickedColeLedgerState {
      tickedColeLedgerState        :: !CC.ChainValidationState
    , untickedColeLedgerTransition :: !ColeTransition
    }
  deriving (Generic, NoThunks)

instance IsLedger (LedgerState ColeBlock) where
  type LedgerErr (LedgerState ColeBlock) = CC.ChainValidationError

  type AuxLedgerEvent (LedgerState ColeBlock) =
    VoidLedgerEvent (LedgerState ColeBlock)

  applyChainTickLedgerResult cfg slotNo ColeLedgerState{..} = pureLedgerResult $
      TickedColeLedgerState {
          tickedColeLedgerState =
            CC.applyChainTick cfg (toColeSlotNo slotNo) coleLedgerState
        , untickedColeLedgerTransition =
            coleLedgerTransition
        }

{-------------------------------------------------------------------------------
  Supporting the various consensus interfaces
-------------------------------------------------------------------------------}

instance ApplyBlock (LedgerState ColeBlock) ColeBlock where
  applyBlockLedgerResult = fmap pureLedgerResult ..: applyColeBlock validationMode
    where
      validationMode = CC.fromBlockValidationMode CC.BlockValidation

  reapplyBlockLedgerResult =
          (pureLedgerResult . validationErrorImpossible)
      ..: applyColeBlock validationMode
    where
      validationMode = CC.fromBlockValidationMode CC.NoBlockValidation

data instance BlockQuery ColeBlock :: Type -> Type where
  GetUpdateInterfaceState :: BlockQuery ColeBlock UPI.State

instance QueryLedger ColeBlock where
  answerBlockQuery _cfg GetUpdateInterfaceState (ExtLedgerState ledgerState _) =
    CC.cvsUpdateState (coleLedgerState ledgerState)

instance SameDepIndex (BlockQuery ColeBlock) where
  sameDepIndex GetUpdateInterfaceState GetUpdateInterfaceState = Just Refl

deriving instance Eq (BlockQuery ColeBlock result)
deriving instance Show (BlockQuery ColeBlock result)

instance ShowQuery (BlockQuery ColeBlock) where
  showResult GetUpdateInterfaceState = show

instance ShowProxy (BlockQuery ColeBlock) where

instance LedgerSupportsPeerSelection ColeBlock where
  getPeers = const []

instance CommonProtocolParams ColeBlock where
  maxHeaderSize = fromIntegral . Update.ppMaxHeaderSize . getProtocolParameters
  maxTxSize     = fromIntegral . Update.ppMaxTxSize     . getProtocolParameters

-- | Return the protocol parameters adopted by the given ledger.
getProtocolParameters :: LedgerState ColeBlock -> Update.ProtocolParameters
getProtocolParameters =
      CC.adoptedProtocolParameters
    . CC.cvsUpdateState
    . coleLedgerState

instance LedgerSupportsProtocol ColeBlock where
  protocolLedgerView _cfg =
        toTickedPBftLedgerView
      . CC.getDelegationMap
      . tickedColeLedgerState

  -- Create a forecast of the delegation state
  --
  -- We can return forecasts for slots in the @[NOW .. NOW+2k)@ window, where
  -- @NOW@ is the slot number of the last block applied to the ledger.
  --
  -- These forecasts will be used to validate future headers, i.e., to check
  -- whether they have been created by the right delegates.
  --
  -- We cannot look more than @2k@ slots ahead, because there might be
  -- delegation state changes present in the blocks between the last block
  -- applied to the ledger and the header to validate that can kick in after
  -- @2k@ slots.
  --
  -- To create a forecast, take the delegation state from the given ledger
  -- state, and apply the updates that should be applied by the given slot.
  ledgerViewForecastAt cfg (ColeLedgerState _tipBlkNo st _) = Forecast at $ \for ->
      toTickedPBftLedgerView <$> if
        | for == lastSlot ->
          return $ CC.getDelegationMap st
        | for < maxFor ->
          return $ CC.previewDelegationMap (toColeSlotNo for) st
        | otherwise ->
          throwError $ OutsideForecastRange {
              outsideForecastAt     = at
            , outsideForecastMaxFor = maxFor
            , outsideForecastFor    = for
            }
    where
      SecurityParam k = genesisSecurityParam cfg
      lastSlot        = fromColeSlotNo $ CC.cvsLastSlot st
      at              = NotOrigin lastSlot

      -- The upper bound is exclusive
      maxFor :: SlotNo
      maxFor = case at of
          Origin      -> SlotNo $ 2 * k
          NotOrigin s -> SlotNo $ unSlotNo s + 1 + (2 * k)

-- | To be used for a Cole-to-X (where X is typically Sophie) chain.
coleEraParams :: Gen.Config -> HardFork.EraParams
coleEraParams genesis = HardFork.EraParams {
      eraEpochSize  = fromColeEpochSlots $ Gen.configEpochSlots genesis
    , eraSlotLength = fromColeSlotLength $ genesisSlotLength genesis
    , eraSafeZone   = HardFork.StandardSafeZone (2 * k)
    }
  where
    SecurityParam k = genesisSecurityParam genesis

-- | Separate variant of 'coleEraParams' to be used for a Cole-only chain.
coleEraParamsNeverHardForks :: Gen.Config -> HardFork.EraParams
coleEraParamsNeverHardForks genesis = HardFork.EraParams {
      eraEpochSize  = fromColeEpochSlots $ Gen.configEpochSlots genesis
    , eraSlotLength = fromColeSlotLength $ genesisSlotLength genesis
    , eraSafeZone   = HardFork.UnsafeIndefiniteSafeZone
    }

instance HasHardForkHistory ColeBlock where
  type HardForkIndices ColeBlock = '[ColeBlock]
  hardForkSummary = neverForksHardForkSummary coleEraParamsNeverHardForks

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Mark computation as validation error free
--
-- Given a 'BlockValidationMode' of 'NoBlockValidation', a call to
-- 'applyColeBlock' shouldn't fail since the ledger layer won't be performing
-- any block validation checks. However, because 'applyColeBlock' can fail in
-- the event it is given a 'BlockValidationMode' of 'BlockValidation', it still
-- /looks/ like it can fail (since its type doesn't change based on the
-- 'ValidationMode') and we must still treat it as such.
validationErrorImpossible :: forall err a. Except err a -> a
validationErrorImpossible = cantBeError . runExcept
  where
    cantBeError :: Either err a -> a
    cantBeError (Left  _) = error "validationErrorImpossible: unexpected error"
    cantBeError (Right a) = a

{-------------------------------------------------------------------------------
  Applying a block

  Most of the work here is done by the ledger layer. We just need to pass
  the right arguments, and maintain the snapshots.
-------------------------------------------------------------------------------}

applyColeBlock :: CC.ValidationMode
                -> LedgerConfig ColeBlock
                -> ColeBlock
                -> TickedLedgerState ColeBlock
                -> Except (LedgerError ColeBlock) (LedgerState ColeBlock)
applyColeBlock validationMode
                cfg
                blk@(ColeBlock raw _ (ColeHash blkHash))
                ls =
    case raw of
      CC.ABOBBlock    raw' -> applyABlock validationMode cfg raw' blkHash blkNo ls
      CC.ABOBBoundary raw' -> applyABoundaryBlock        cfg raw'         blkNo ls
  where
    blkNo :: BlockNo
    blkNo = blockNo blk

applyABlock :: CC.ValidationMode
            -> Gen.Config
            -> CC.ABlock ByteString
            -> CC.HeaderHash
            -> BlockNo
            -> Ticked (LedgerState (ColeBlock))
            -> Except (LedgerError ColeBlock) (LedgerState ColeBlock)
applyABlock validationMode cfg blk blkHash blkNo TickedColeLedgerState{..} = do
    st' <- CC.validateBlock cfg validationMode blk blkHash tickedColeLedgerState

    let updState :: UPI.State
        updState = CC.cvsUpdateState st'

        -- Transition info as it would look like if all entries were new
        ifNew :: Map Update.ProtocolVersion BlockNo
        ifNew = Map.fromList $ map aux (UPI.candidateProtocolUpdates updState)
          where
            aux :: UPE.CandidateProtocolUpdate
                -> (Update.ProtocolVersion, BlockNo)
            aux candidate = (UPE.cpuProtocolVersion candidate, blkNo)

        transition' :: ColeTransition
        transition' =
            case untickedColeLedgerTransition of
              ColeTransitionInfo oldEntries -> ColeTransitionInfo $
                -- Candidates that have /just/ become candidates
                let newEntries :: Map Update.ProtocolVersion BlockNo
                    newEntries = ifNew `Map.difference` oldEntries

                -- Remove any entries that aren't candidates anymore
                in (oldEntries `Map.intersection` ifNew) `Map.union` newEntries

    return ColeLedgerState {
          coleLedgerTipBlockNo = NotOrigin blkNo
        , coleLedgerState      = st'
        , coleLedgerTransition = transition'
        }

-- | Apply boundary block
--
-- Since boundary blocks don't modify the delegation state, they also don't
-- modify the delegation history.
applyABoundaryBlock :: Gen.Config
                    -> CC.ABoundaryBlock ByteString
                    -> BlockNo
                    -> Ticked (LedgerState ColeBlock)
                    -> Except (LedgerError ColeBlock) (LedgerState ColeBlock)
applyABoundaryBlock cfg blk blkNo TickedColeLedgerState{..} = do
    st' <- CC.validateBoundary cfg blk tickedColeLedgerState
    return ColeLedgerState {
        coleLedgerTipBlockNo = NotOrigin blkNo
      , coleLedgerState      = st'
      , coleLedgerTransition = untickedColeLedgerTransition
      }

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeColeAnnTip :: AnnTip ColeBlock -> Encoding
encodeColeAnnTip = encodeAnnTipIsEBB encodeColeHeaderHash

decodeColeAnnTip :: Decoder s (AnnTip ColeBlock)
decodeColeAnnTip = decodeAnnTipIsEBB decodeColeHeaderHash

encodeColeExtLedgerState :: ExtLedgerState ColeBlock -> Encoding
encodeColeExtLedgerState = encodeExtLedgerState
    encodeColeLedgerState
    encodeColeChainDepState
    encodeColeAnnTip

encodeColeHeaderState :: HeaderState ColeBlock -> Encoding
encodeColeHeaderState = encodeHeaderState
    encodeColeChainDepState
    encodeColeAnnTip

-- | Encode transition info
--
-- We encode the absence of any info separately. This gives us a bit more
-- wiggle room to change our mind about what we store in snapshots, as they
-- typically don't contain any transition info.
--
-- Implementation note: we should have encoded the absence of data with the
-- inclusion of a list length. We didn't, so the decoder is a bit awkward :/
--
-- TODO: If we break compatibility anyway, we might decide to clean this up.
encodeColeTransition :: ColeTransition -> Encoding
encodeColeTransition (ColeTransitionInfo bNos)
  | Map.null bNos = CBOR.encodeWord8 0
  | otherwise     =
         CBOR.encodeListLen (fromIntegral (Map.size bNos))
      <> mconcat (map aux (Map.toAscList bNos))
  where
    aux :: (Update.ProtocolVersion, BlockNo) -> Encoding
    aux (Update.ProtocolVersion { pvMajor, pvSentry }, bno) = mconcat [
          CBOR.encodeListLen 3
        , encode pvMajor
        , encode pvSentry
        , encode bno
        ]

-- | Decode Cole transition info
--
-- See comments for 'encodeColeTransition'.
decodeColeTransition :: Decoder s ColeTransition
decodeColeTransition = do
    ttype <- CBOR.peekTokenType
    fmap ColeTransitionInfo $ case ttype of
      CBOR.TypeUInt -> do
        tag <- CBOR.decodeWord8
        case tag of
          0          -> return $ Map.empty
          _otherwise -> fail "decodeColeTransition: unexpected tag"
      CBOR.TypeListLen -> do
        size <- CBOR.decodeListLen
        Map.fromAscList <$> replicateM size aux
      _otherwise ->
        fail "decodeColeTransition: unexpected token type"
  where
    aux :: Decoder s (Update.ProtocolVersion, BlockNo)
    aux = do
        enforceSize "decodeColeTransition.aux" 3
        pvMajor <- decode
        pvSentry <- decode
        bno     <- decode
        return (Update.ProtocolVersion { pvMajor, pvSentry}, bno)

encodeColeLedgerState :: LedgerState ColeBlock -> Encoding
encodeColeLedgerState ColeLedgerState{..} = mconcat [
      encodeListLen 3
    , encode coleLedgerTipBlockNo
    , encode coleLedgerState
    , encodeColeTransition coleLedgerTransition
    ]

decodeColeLedgerState :: Decoder s (LedgerState ColeBlock)
decodeColeLedgerState = do
    enforceSize "ColeLedgerState" 3
    ColeLedgerState
      <$> decode
      <*> decode
      <*> decodeColeTransition

encodeColeQuery :: BlockQuery ColeBlock result -> Encoding
encodeColeQuery query = case query of
    GetUpdateInterfaceState -> CBOR.encodeWord8 0

decodeColeQuery :: Decoder s (SomeSecond BlockQuery ColeBlock)
decodeColeQuery = do
    tag <- CBOR.decodeWord8
    case tag of
      0 -> return $ SomeSecond GetUpdateInterfaceState
      _ -> fail $ "decodeColeQuery: invalid tag " <> show tag

encodeColeResult :: BlockQuery ColeBlock result -> result -> Encoding
encodeColeResult query = case query of
    GetUpdateInterfaceState -> toCBOR

decodeColeResult :: BlockQuery ColeBlock result
                  -> forall s. Decoder s result
decodeColeResult query = case query of
    GetUpdateInterfaceState -> fromCBOR
