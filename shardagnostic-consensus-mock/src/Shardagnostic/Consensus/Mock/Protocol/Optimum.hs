{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}

-- | Proof of concept implementation of Optimum
module Shardagnostic.Consensus.Mock.Protocol.Optimum (
    HotKey (..)
  , HotKeyEvolutionError (..)
  , Optimum
  , OptimumChainDepState (..)
  , OptimumEvolvingStake (..)
  , OptimumExtraFields (..)
  , OptimumFields (..)
  , OptimumParams (..)
  , emptyOptimumEvolvingStake
  , evolveKey
  , forgeOptimumFields
    -- * Tags
  , OptimumCrypto (..)
  , OptimumMockCrypto
  , OptimumStandardCrypto
  , OptimumValidateView (..)
  , OptimumValidationError (..)
  , optimumValidateView
    -- * Type instances
  , BlockInfo (..)
  , ConsensusConfig (..)
  , Ticked (..)
  ) where

import           Bcc.Binary (FromCBOR (..), ToCBOR (..), serializeEncoding')
import           Codec.CBOR.Decoding (decodeListLenOf)
import           Codec.CBOR.Encoding (encodeListLen)
import           Codec.Serialise (Serialise (..))
import           Control.Monad (unless)
import           Control.Monad.Except (throwError)
import           Data.Kind (Type)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Typeable
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks (..))
import           Numeric.Natural

import           Bcc.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Bcc.Crypto.Hash.Class (HashAlgorithm (..), hashToBytes,
                     hashWithSerialiser, sizeHash)
import           Bcc.Crypto.Hash.SHA256 (SHA256)
import           Bcc.Crypto.KES.Class
import           Bcc.Crypto.KES.Mock
import           Bcc.Crypto.KES.Simple
import           Bcc.Crypto.Util
import           Bcc.Crypto.VRF.Class
import           Bcc.Crypto.VRF.Mock (MockVRF)
import           Bcc.Crypto.VRF.Simple (SimpleVRF)
import           Bcc.Slotting.EpochInfo

import           Data.Maybe (fromMaybe)
import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Mock.Ledger.Stake
import           Shardagnostic.Consensus.NodeId (CoreNodeId (..))
import           Shardagnostic.Consensus.Protocol.Abstract
import           Shardagnostic.Consensus.Protocol.Signed
import           Shardagnostic.Consensus.Util.Condense

-- The Optimum paper can be located at https://ia.cr/2017/573
--
-- A block as defined in Optimum (section 3, definition 2, then extended in Fig 9)
-- consist of a tuple:
--
-- > B = (slⱼ, st, d, B_πj, ρ, σⱼ)
--
-- where
-- - slⱼ: the slot at which the block was generated.
--        Named 'simpleSlotNo' in the 'SimpleStdHeader' block header.
--        As far as consensus is concerned this will be accessed through
--        'biSlot' in each block in the 'ChainDepState'.
--
-- - st: state, a string ∈ {0,1}^λ, which holds the hash of the previous block.
--       Named 'simplePrev' in the 'SimpleStdHeader' block header.
--
-- - d: data (transaction data in most cases).
--      Named 'simpleBody' inside 'SimpleBlock'.
--
-- - B_πj: block proof consisting of (Uᵢ, y, π).
--         Named 'optimumY' inside 'OptimumExtraFields'.
--      - y: a VRF output used to confirm that Uᵢ was the slot leader.
--      - π: the VRF proof of the above value.
--
--      > (y, π) ≜ VRF_evalProve(η, slⱼ, TEST) see Fig 9
--
-- - ρ: the block nonce consisting of (ρ_y, ρ_π), to capture entropy from the
--      block forging process.
--      Named 'optimumRho' inside 'OptimumExtraFields'.
--      - ρ_y: a VRF output used to confirm this block captured all the previous
--             entropy.
--      - ρ_π: the VRF proof of the above value.
--
--      > (ρ_y, ρ_π) ≜ VRF_evalProve(η, slⱼ, NONCE) see Fig 9
--
-- - σⱼ: a signature on (st, d, slⱼ, B_πj, ρ) with the signing key
--       for the slot slⱼ for the stakeholder Uᵢ.
--       Named 'optimumSignature' in 'OptimumFields'.
--
-- Protocol parameters:
-- - k: maximum number of blocks we can rollback.
--      Named 'optimumSecurityParam' in 'OptimumParams'.
-- - R: number of slots per epoch.
--      Named 'optimumSlotsPerEpoch' in 'OptimumParams'.
-- - f: the active slots coefficient, specifies roughly the proportion of
--      occupied slots per epoch.
--      Named 'optimumLeaderF' in 'OptimumParams'.
--
-- Some values you will encounter:
-- - η: The epoch's nonce. Captures entropy from the block chain. See Fig 8 in
--      optimum paper for where it is used and Fig 10 for how it is defined.
--      Defined as the hash of the η from the previous epoch, this epoch number
--      and the ρ of the first 2/3 of the blocks in the previous epoch.
--      Commonly named through the code as 'eta'.
--
--      > η_e ≜ HASH(η_{e-1} || e || ρ_{e-1,0} ... ρ_{e-1, 2R/3})
--
-- - Tᵢ: the leader threshold for a specific stakeholder considering its
--       relative stake (therefore depending on the slot). Defined in the Optimum
--       paper in Figure 4 using the definition for ϕ_f(αᵢ) from section 3.3.
--       Named 't' in the code but essentially computed by 'leaderThreshold' in
--       'rhoYT'.
--
--       > Tᵢ ≜ 2^(l_VRF) * pᵢ
--       > pᵢ = ϕ_f(αᵢ) ≜ 1 - (1 - f)^(αᵢ)

{-------------------------------------------------------------------------------
  Fields required by Optimum in the header
-------------------------------------------------------------------------------}

-- | The fields that Optimum required in the header
data OptimumFields crypto typeBeingSigned = OptimumFields {
      optimumSignature   :: SignedKES (OptimumKES crypto) typeBeingSigned
    , optimumExtraFields :: OptimumExtraFields crypto
    }
  deriving (Generic)

instance (OptimumCrypto c, Typeable toSign) => NoThunks (OptimumFields c toSign)

deriving instance OptimumCrypto c => Show (OptimumFields c toSign)
deriving instance OptimumCrypto c => Eq   (OptimumFields c toSign)

-- | Fields that should be included in the signature
data OptimumExtraFields c = OptimumExtraFields {
      optimumCreator :: CoreNodeId
    , optimumRho     :: CertifiedVRF (OptimumVRF c) (Natural, SlotNo, VRFType)
    , optimumY       :: CertifiedVRF (OptimumVRF c) (Natural, SlotNo, VRFType)
    }
  deriving (Generic)

instance OptimumCrypto c => NoThunks (OptimumExtraFields c)

deriving instance OptimumCrypto c => Show (OptimumExtraFields c)
deriving instance OptimumCrypto c => Eq   (OptimumExtraFields c)

-- | A validate view is an association from the (@signed@) value to the
-- @OptimumFields@ that contains the signature that sign it.
--
-- In this mock implementation, this could have been simplified to use
-- @SignedSimpleOptimum@ but from the consensus point of view, it is not relevant
-- which actual value is being signed, that's why we use the existential.
data OptimumValidateView c =
    forall signed. Bcc.Crypto.KES.Class.Signable (OptimumKES c) signed
                => OptimumValidateView (OptimumFields c signed) signed

-- | Convenience constructor for 'OptimumValidateView'
optimumValidateView :: ( SignedHeader hdr
                     , Bcc.Crypto.KES.Class.Signable (OptimumKES c) (Signed hdr)
                     )
                  => (hdr -> OptimumFields c (Signed hdr))
                  -> (hdr -> OptimumValidateView c)
optimumValidateView getFields hdr =
    OptimumValidateView (getFields hdr) (headerSigned hdr)

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

-- | The key used for the given period or a stub Poisoned value.
--
-- A key will be poisoned if it failed to evolve by @updateKES@, and will remain
-- poisoned forever after that.
data HotKey c =
    HotKey
      !Period  -- ^ Absolute period of the KES key
      !(SignKeyKES (OptimumKES c))
  | HotKeyPoisoned
  deriving (Generic)

instance OptimumCrypto c => NoThunks (HotKey c)
deriving instance OptimumCrypto c => Show (HotKey c)

-- | The 'HotKey' could not be evolved to the given 'Period'.
newtype HotKeyEvolutionError = HotKeyEvolutionError Period
  deriving (Show)

-- | To be used in conjunction with, e.g., 'updateMVar'.
--
-- NOTE: when the key's period is after the target period, we shouldn't use
-- it, but we currently do. In real TOptimum we check this in
-- 'toptimumCheckCanForge'.
evolveKey ::
     OptimumCrypto c
  => SlotNo
  -> HotKey c
  -> (HotKey c, UpdateInfo (HotKey c) HotKeyEvolutionError)
evolveKey slotNo hotKey = case hotKey of
    HotKey keyPeriod oldKey
      | keyPeriod >= targetPeriod
      -> (hotKey, Updated hotKey)
      | otherwise
      -> case updateKES () oldKey keyPeriod of
           Nothing     ->
             (HotKeyPoisoned, UpdateFailed $ HotKeyEvolutionError targetPeriod)
           Just newKey ->
             evolveKey slotNo (HotKey (keyPeriod + 1) newKey)
    HotKeyPoisoned ->
      (HotKeyPoisoned, UpdateFailed $ HotKeyEvolutionError targetPeriod)
  where
   targetPeriod :: Period
   targetPeriod = fromIntegral $ unSlotNo slotNo

-- | Create a OptimumFields using a proof, a key and the data to be signed.
--
-- It is done by signing whatever is extracted from the extra fields by @mkToSign@
-- and storing the signature and the extra fields on a @OptimumFields@.
forgeOptimumFields :: ( OptimumCrypto c
                    , Bcc.Crypto.KES.Class.Signable (OptimumKES c) toSign
                    , HasCallStack
                    )
                 => OptimumProof c
                 -> HotKey c
                 -> (OptimumExtraFields c -> toSign)
                 -> OptimumFields c toSign
forgeOptimumFields OptimumProof{..} hotKey mkToSign =
    case hotKey of
      HotKey kesPeriod key -> OptimumFields {
          optimumSignature   = signedKES () kesPeriod (mkToSign fieldsToSign) key
        , optimumExtraFields = fieldsToSign
        }
      HotKeyPoisoned -> error "trying to sign with a poisoned key"
  where
    fieldsToSign = OptimumExtraFields {
        optimumCreator = optimumLeader
      , optimumRho     = optimumProofRho
      , optimumY       = optimumProofY
      }

{-------------------------------------------------------------------------------
  Mock stake distribution
-------------------------------------------------------------------------------}

-- | An association from epoch to stake distributions.
--
-- Should be used when checking if someone is the leader of a particular slot.
-- This is sufficiently good for a mock protocol as far as consensus is
-- concerned. It is not strictly necessary that the stake distribution is
-- computed from previous epochs, as we just need to consider that:
--
-- 1) an attacker cannot influence it.
-- 2) all the nodes agree on the same value for each Slot.
--
-- Each pair stores the stake distribution established by the end of the epoch
-- in the first item of the pair. See 'latestEvolvedStakeDistAsOfEpoch' for the
-- intended interface.
--
-- If no value is returned, that means we are checking the stake before any
-- changes have happened so we should consult instead the 'optimumInitialStake'.
newtype OptimumEvolvingStake = OptimumEvolvingStake (Map EpochNo StakeDist)
  deriving stock Show
  deriving newtype NoThunks

emptyOptimumEvolvingStake :: OptimumEvolvingStake
emptyOptimumEvolvingStake = OptimumEvolvingStake Map.empty

latestEvolvedStakeDistAsOfEpoch :: OptimumEvolvingStake -> EpochNo -> Maybe StakeDist
latestEvolvedStakeDistAsOfEpoch (OptimumEvolvingStake x) e =
  fmap snd . Map.lookupMax . fst $ Map.split e x

{-------------------------------------------------------------------------------
  Optimum specific types
-------------------------------------------------------------------------------}

-- |The two VRF invocation modes, NONCE (rho) and TEST (y). See the comment at
-- the top of the module for an explanation of these.
data VRFType = NONCE | TEST
    deriving (Show, Eq, Ord, Generic, NoThunks)

instance Serialise VRFType

instance ToCBOR VRFType where
  -- This is a cheat, and at some point we probably want to decide on Serialise/ToCBOR
  toCBOR = encode

-- |Proofs certifying ρ and y for a given slot and eta.
data OptimumProof c = OptimumProof {
      optimumProofRho :: CertifiedVRF (OptimumVRF c) (Natural, SlotNo, VRFType)
    , optimumProofY   :: CertifiedVRF (OptimumVRF c) (Natural, SlotNo, VRFType)
    , optimumLeader   :: CoreNodeId
    }

-- | An error that can arise during validation
data OptimumValidationError c =
      OptimumInvalidSlot SlotNo SlotNo
    | OptimumUnknownCoreId CoreNodeId
    | OptimumInvalidSig String (VerKeyKES (OptimumKES c)) Natural (SigKES (OptimumKES c))
    | OptimumInvalidCert (VerKeyVRF (OptimumVRF c)) (Natural, SlotNo, VRFType) Natural (CertVRF (OptimumVRF c))
    | OptimumInsufficientStake Double Natural
    deriving (Generic)

-- We override 'showTypeOf' to make sure to show @c@
instance OptimumCrypto c => NoThunks (OptimumValidationError c) where
  showTypeOf _ = show $ typeRep (Proxy @(OptimumValidationError c))

deriving instance OptimumCrypto c => Show (OptimumValidationError c)
deriving instance OptimumCrypto c => Eq   (OptimumValidationError c)

data BlockInfo c = BlockInfo
    { biSlot :: !SlotNo
    , biRho  :: !(CertifiedVRF (OptimumVRF c) (Natural, SlotNo, VRFType))
    }
  deriving (Generic)

deriving instance OptimumCrypto c => Show     (BlockInfo c)
deriving instance OptimumCrypto c => Eq       (BlockInfo c)
deriving instance OptimumCrypto c => NoThunks (BlockInfo c)

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

-- | An uninhabited type representing the Optimum protocol.
data Optimum c

-- | Optimum parameters that are node independent
data OptimumParams = OptimumParams {
      optimumLeaderF       :: !Double
      -- ^ f, the active slots coefficient, defined in 3.3 in the Optimum paper.
    , optimumSecurityParam :: !SecurityParam
      -- ^ k, maximum number of blocks we can rollback
    , optimumSlotsPerEpoch :: !Word64
      -- ^ R, slots in each epoch, defined in section 3 in the Optimum paper.
    }
  deriving (Generic, NoThunks)

-- | The configuration that will be provided to every node when running the
-- MockOptimum protocol.
data instance ConsensusConfig (Optimum c) = OptimumConfig
  { optimumParams        :: !OptimumParams
  , optimumInitialEta    :: !Natural
  , optimumInitialStake  :: !StakeDist
  , optimumEvolvingStake :: !OptimumEvolvingStake
  , optimumSignKeyVRF    :: !(SignKeyVRF (OptimumVRF c))
  , optimumVerKeys       :: !(Map CoreNodeId (VerKeyKES (OptimumKES c), VerKeyVRF (OptimumVRF c)))
  }
  deriving (Generic)

instance OptimumCrypto c => NoThunks (ConsensusConfig (Optimum c))

slotEpoch :: ConsensusConfig (Optimum c) -> SlotNo -> EpochNo
slotEpoch OptimumConfig{..} s =
    fixedEpochInfoEpoch (EpochSize optimumSlotsPerEpoch) s
  where
    OptimumParams{..} = optimumParams

epochFirst :: ConsensusConfig (Optimum c) -> EpochNo -> SlotNo
epochFirst OptimumConfig{..} e =
    fixedEpochInfoFirst (EpochSize optimumSlotsPerEpoch) e
  where
    OptimumParams{..} = optimumParams

-- |The chain dependent state, in this case as it is a mock, we just will store
-- a list of BlockInfos that allow us to look into the past.
newtype OptimumChainDepState c = OptimumChainDepState {
      optimumHistory :: [BlockInfo c]
    }
  deriving stock   (Eq, Show)
  deriving newtype (NoThunks, Serialise)

infosSlice :: SlotNo -> SlotNo -> [BlockInfo c] -> [BlockInfo c]
infosSlice from to xs = takeWhile (\b -> biSlot b >= from)
                      $ dropWhile (\b -> biSlot b > to) xs

infosEta :: forall c. (OptimumCrypto c)
         => ConsensusConfig (Optimum c)
         -> [BlockInfo c]
         -> EpochNo
         -> Natural
infosEta l _  0 =
    optimumInitialEta l
infosEta l@OptimumConfig{optimumParams = OptimumParams{..}} xs e =
    let e'   = e - 1
        -- the η from the previous epoch
        eta' = infosEta l xs e'
        -- the first slot in previous epoch
        from = epochFirst l e'
        -- 2/3 of the slots per epoch
        n    = div (2 * optimumSlotsPerEpoch) 3
        -- the last of the 2/3 of slots in this epoch
        to   = SlotNo $ unSlotNo from + n
        -- the list of rhos from the first block in this epoch until the one at
        -- 2/3 of the slots. Note it is reversed, i.e. start at the oldest.
        rhos = reverse [biRho b | b <- infosSlice from to xs]
    in  bytesToNatural
          . hashToBytes
          $ hashWithSerialiser @(OptimumHash c) toCBOR (eta', e, rhos)

-- | Ticking the Optimum chain dep state has no effect
--
-- For the real Optimum implementation, ticking is crucial, as it determines the
-- point where the "nonce under construction" is swapped out for the "active"
-- nonce. However, for the mock implementation, we keep the full history, and
-- choose the right nonce from that; this means that ticking has no effect.
--
-- We do however need access to the ticked stake distribution.
data instance Ticked (OptimumChainDepState c) = TickedOptimumChainDepState {
      tickedOptimumLedgerView      :: Ticked (LedgerView (Optimum c))
      -- ^ The ticked ledger view.
    , untickedOptimumChainDepState :: OptimumChainDepState c
      -- ^ The unticked chain dependent state, containing the full history.
    }

instance OptimumCrypto c => ConsensusProtocol (Optimum c) where

  protocolSecurityParam = optimumSecurityParam . optimumParams

  type LedgerView    (Optimum c) = ()
  type IsLeader      (Optimum c) = OptimumProof           c
  type ValidationErr (Optimum c) = OptimumValidationError c
  type ValidateView  (Optimum c) = OptimumValidateView    c
  type ChainDepState (Optimum c) = OptimumChainDepState   c
  type CanBeLeader   (Optimum c) = CoreNodeId

  checkIsLeader cfg@OptimumConfig{..} nid slot (TickedOptimumChainDepState _u  cds) =
      -- See Figure 4 of the Optimum paper.
      -- In order to be leader, y must be < Tᵢ
      if fromIntegral (getOutputVRFNatural (certifiedOutput y)) < t
      then Just OptimumProof {
               optimumProofRho = rho
             , optimumProofY   = y
             , optimumLeader   = nid
             }
      else Nothing
    where
      (rho', y', t) = rhoYT cfg (optimumHistory cds) slot nid
      rho = evalCertified () rho' optimumSignKeyVRF
      y   = evalCertified () y'   optimumSignKeyVRF

  tickChainDepState _ lv _ = TickedOptimumChainDepState lv

  updateChainDepState cfg@OptimumConfig{..}
                      (OptimumValidateView OptimumFields{..} toSign)
                      slot
                      (TickedOptimumChainDepState TickedTrivial cds) = do
    let OptimumExtraFields {..} = optimumExtraFields
        nid = optimumCreator

    -- check that the new block advances time
    case optimumHistory cds of
        (c : _)
            | biSlot c >= slot -> throwError $ OptimumInvalidSlot slot (biSlot c)
        _                      -> return ()

    -- check that block creator is a known core node
    (vkKES, vkVRF) <- case Map.lookup nid optimumVerKeys of
        Nothing  -> throwError $ OptimumUnknownCoreId nid
        Just vks -> return vks

    -- verify block signature
    case verifySignedKES
           ()
           vkKES
           (fromIntegral $ unSlotNo slot)
           toSign
           optimumSignature of
       Right () -> return ()
       Left err -> throwError $ OptimumInvalidSig
                                  err
                                  vkKES
                                  (fromIntegral $ unSlotNo slot)
                                  (getSig optimumSignature)

    let (rho', y', t) = rhoYT cfg (optimumHistory cds) slot nid

    -- verify rho proof
    unless (verifyCertified () vkVRF rho' optimumRho) $
        throwError $ OptimumInvalidCert
            vkVRF
            rho'
            (getOutputVRFNatural (certifiedOutput optimumRho))
            (certifiedProof optimumRho)

    -- verify y proof
    unless (verifyCertified () vkVRF y' optimumY) $
        throwError $ OptimumInvalidCert
            vkVRF
            y'
            (getOutputVRFNatural (certifiedOutput optimumY))
            (certifiedProof optimumY)

    -- verify stake
    unless (fromIntegral (getOutputVRFNatural (certifiedOutput optimumY)) < t) $
        throwError $ OptimumInsufficientStake t $
                       getOutputVRFNatural (certifiedOutput optimumY)

    -- "store" a block by adding it to the chain dependent state
    let !bi = BlockInfo
            { biSlot  = slot
            , biRho   = optimumRho
            }

    return $ OptimumChainDepState $ bi : optimumHistory cds

  reupdateChainDepState _
                        (OptimumValidateView OptimumFields{..} _)
                        slot
                        (TickedOptimumChainDepState TickedTrivial cds) =
    let OptimumExtraFields{..} = optimumExtraFields
        !bi = BlockInfo
            { biSlot  = slot
            , biRho   = optimumRho
            }
    in OptimumChainDepState $ bi : optimumHistory cds

  -- (Standard) Optimum uses the standard chain selection rule, so no need to
  -- override (though see note regarding clock skew).

-- | Probability for stakeholder Uᵢ to be elected in slot
-- slⱼ considering its relative stake αᵢ.
phi :: ConsensusConfig (Optimum c) -> Rational -> Double
phi OptimumConfig{..} alpha = 1 - (1 - optimumLeaderF) ** fromRational alpha
  where
    OptimumParams{..} = optimumParams

-- | Compute Tᵢ for a given stakeholder @n@ at a @SlotNo@. Will be computed from
-- 'optimumEvolvingStake' (or taken from 'optimumInitialStake' if checking epoch 0).
leaderThreshold :: forall c. OptimumCrypto c
                => ConsensusConfig (Optimum c)
                -> [BlockInfo c]
                -> SlotNo
                -> CoreNodeId
                -> Double
leaderThreshold config _blockInfos s n =
    let
      alpha = stakeWithDefault 0 n
        $ fromMaybe (optimumInitialStake config)
        $ latestEvolvedStakeDistAsOfEpoch (optimumEvolvingStake config) (slotEpoch config s)
    in
      -- 2^(l_VRF * 8) * ϕ_f(αᵢ)
      -- the 8 factor converts from bytes to bits.
      2 ^ (sizeHash (Proxy :: Proxy (OptimumHash c)) * 8) * phi config alpha

-- |Compute the rho, y and Tᵢ parameters for a given slot.
rhoYT :: OptimumCrypto c
      => ConsensusConfig (Optimum c)
      -> [BlockInfo c]
      -> SlotNo
      -> CoreNodeId
      -> ( (Natural, SlotNo, VRFType)
         , (Natural, SlotNo, VRFType)
         , Double
         )
rhoYT st xs s nid =
    let e   = slotEpoch st s
        eta = infosEta st xs e
        rho = (eta, s, NONCE)
        y   = (eta, s, TEST)
        t   = leaderThreshold st xs s nid
    in  (rho, y, t)

{-------------------------------------------------------------------------------
  Crypto models
-------------------------------------------------------------------------------}

class ( KESAlgorithm  (OptimumKES  c)
      , VRFAlgorithm  (OptimumVRF  c)
      , HashAlgorithm (OptimumHash c)
      , Typeable c
      , Typeable (OptimumVRF c)
      , Condense (SigKES (OptimumKES c))
      , Bcc.Crypto.VRF.Class.Signable (OptimumVRF c) (Natural, SlotNo, VRFType)
      , ContextKES (OptimumKES c) ~ ()
      , ContextVRF (OptimumVRF c) ~ ()
      ) => OptimumCrypto (c :: Type) where
  type family OptimumKES  c :: Type
  type family OptimumVRF  c :: Type
  type family OptimumHash c :: Type

data OptimumStandardCrypto
data OptimumMockCrypto

instance OptimumCrypto OptimumStandardCrypto where
  type OptimumKES  OptimumStandardCrypto = SimpleKES Ed448DSIGN 1000
  type OptimumVRF  OptimumStandardCrypto = SimpleVRF
  type OptimumHash OptimumStandardCrypto = SHA256

instance OptimumCrypto OptimumMockCrypto where
  type OptimumKES  OptimumMockCrypto = MockKES 10000
  type OptimumVRF  OptimumMockCrypto = MockVRF
  type OptimumHash OptimumMockCrypto = SHA256

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance OptimumCrypto c => Condense (OptimumFields c toSign) where
   condense OptimumFields{..} = condense optimumSignature

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance OptimumCrypto c => Serialise (BlockInfo c) where
  encode BlockInfo {..} = mconcat
    [ encodeListLen 2
    , encode biSlot
    , toCBOR biRho
    ]
  decode = do
    decodeListLenOf 2
    biSlot  <- decode
    biRho   <- fromCBOR
    return BlockInfo {..}

instance SignableRepresentation (Natural, SlotNo, VRFType) where
  getSignableRepresentation = serializeEncoding' . toCBOR
