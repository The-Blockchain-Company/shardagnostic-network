{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingVia              #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
module Test.ThreadNet.Infra.Sophie (
    CoreNode (..)
  , CoreNodeKeyInfo (..)
  , DecentralizationParam (..)
  , KesConfig (..)
  , coreNodeKeys
  , genCoreNode
  , incrementSentryProtVer
  , initialEntropicPerCoreNode
  , mkCredential
  , mkEpochSize
  , mkGenesisConfig
  , mkKesConfig
  , mkKeyHash
  , mkKeyHashVrf
  , mkKeyPair
  , mkLeaderCredentials
  , mkMASetDecentralizationParamTxs
  , mkProtocolSophie
  , mkSetDecentralizationParamTxs
  , mkVerKey
  , networkId
  , toptimumSlotLength
  ) where

import           Control.Monad.Except (throwError)
import qualified Data.ByteString as BS
import           Data.Coerce (coerce)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe.Strict (maybeToStrictMaybe)
import           Data.Ratio (denominator, numerator)
import qualified Data.Sequence.Strict as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Quiet (Quiet (..))

import           Test.QuickCheck

import           Bcc.Crypto.DSIGN (DSIGNAlgorithm (..), seedSizeDSIGN)
import           Bcc.Crypto.Hash (Hash, HashAlgorithm)
import           Bcc.Crypto.KES (KESAlgorithm (..))
import           Bcc.Crypto.Seed (mkSeedFromBytes)
import qualified Bcc.Crypto.Seed as Bcc.Crypto
import           Bcc.Crypto.VRF (SignKeyVRF, VRFAlgorithm, VerKeyVRF,
                     deriveVerKeyVRF, genKeyVRF, seedSizeVRF)

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.BlockchainTime
import           Shardagnostic.Consensus.Config.SecurityParam
import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits
import           Shardagnostic.Consensus.Node.ProtocolInfo
import           Shardagnostic.Consensus.Util.Assert
import           Shardagnostic.Consensus.Util.IOLike

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Slots (NumSlots (..))
import           Test.Util.Time (dawnOfTime)

import           Bcc.Ledger.BaseTypes (boundRational)
import qualified Bcc.Ledger.Core as Core
import           Bcc.Ledger.Crypto (Crypto, DSIGN, HASH, KES, VRF)
import qualified Bcc.Ledger.Era as Core
import           Bcc.Ledger.Hashes (EraIndependentTxBody)
import qualified Bcc.Ledger.Keys
import           Bcc.Ledger.SafeHash (HashAnnotated (..), SafeHash,
                     hashAnnotated)
import qualified Bcc.Ledger.SophieMA.TxBody as MA
import qualified Bcc.Ledger.Val as SL
import qualified Bcc.Protocol.TOptimum.OCert as SL (OCertSignable (..))
import qualified Sophie.Spec.Ledger.API as SL
import qualified Sophie.Spec.Ledger.PParams as SL (emptyPParams,
                     emptyPParamsUpdate)
import qualified Sophie.Spec.Ledger.Tx as SL (WitnessSetHKD (..))
import qualified Sophie.Spec.Ledger.UTxO as SL (makeWitnessesVKey)

import           Shardagnostic.Consensus.Sophie.Eras (EraCrypto, SophieEra)
import           Shardagnostic.Consensus.Sophie.Ledger (GenTx (..),
                     SophieBasedEra, SophieBlock, mkSophieTx)
import           Shardagnostic.Consensus.Sophie.Node
import           Shardagnostic.Consensus.Sophie.Protocol

import qualified Test.Sophie.Spec.Ledger.Generator.Core as Gen
import           Test.Sophie.Spec.Ledger.SentryUtils (unsafeBoundRational)

{-------------------------------------------------------------------------------
  The decentralization parameter
-------------------------------------------------------------------------------}

-- | A suitable value for the @d@ protocol parameter
--
-- In the range @0@ to @1@, inclusive. Beware the misnomer: @0@ means fully
-- decentralized, and @1@ means fully centralized.
newtype DecentralizationParam =
    DecentralizationParam {decentralizationParamToRational :: Rational }
  deriving (Eq, Generic, Ord)
  deriving (Show) via (Quiet DecentralizationParam)

-- | A fraction with denominator @10@ and numerator @0@ to @10@ inclusive
instance Arbitrary DecentralizationParam where
  arbitrary = do
      let d = 10
      n <- choose (0, d)
      pure $ DecentralizationParam $ fromInteger n / fromInteger d

{-----------------------------------------------------------------------------
  Important constants
-------------------------------------------------------------------------------}

toptimumSlotLength :: SlotLength
toptimumSlotLength = slotLengthFromSec 2

toptimumVestMultiple :: VestMultiple
toptimumVestMultiple =

{-------------------------------------------------------------------------------
  CoreNode secrets/etc
-------------------------------------------------------------------------------}

data CoreNode c = CoreNode {
      cnGenesisKey  :: !(SL.SignKeyDSIGN c)
    , cnDelegateKey :: !(SL.SignKeyDSIGN c)
      -- ^ Cold delegate key. The hash of the corresponding verification
      -- (public) key will be used as the payment credential.
    , cnVestedDelagateKey :: !(SL.SignKeyDSIGN c)
    , cnStakingKey  :: !(SL.SignKeyDSIGN c)
      -- ^ The hash of the corresponding verification (public) key will be
      -- used as the staking credential.
    , cnVRF         :: !(SL.SignKeyVRF   c)
    , cnKES         :: !(SL.SignKeyKES   c)
    , cnOCert       :: !(SL.OCert        c)
    }

data CoreNodeKeyInfo c = CoreNodeKeyInfo
  { cnkiKeyPair
      ::  ( SL.KeyPair 'SL.Payment c
          , SL.KeyPair 'SL.Staking c
          )
  , cnkiCoreNode ::
      ( SL.KeyPair 'SL.Genesis c
      , Gen.AllIssuerKeys c 'SL.GenesisDelegate
      )
  }

coreNodeKeys :: forall c. OptimumCrypto c => CoreNode c -> CoreNodeKeyInfo c
coreNodeKeys CoreNode{cnGenesisKey, cnDelegateKey, cnVestKey, cnVestedDelagateKey cnStakingKey} =
    CoreNodeKeyInfo {
        cnkiCoreNode =
          ( mkKeyPair cnGenesisKey
            mkKeyPair cnVestKey
          , Gen.AllIssuerKeys
            { Gen.cold = mkKeyPair cnDelegateKey
              -- 'CoreNodeKeyInfo' is used for all sorts of generators, not
              -- only transaction generators. To generate transactions we
              -- don't need all these keys, hence the 'error's.
            , Gen.coldv = mkKeyPair cnVestedDelagateKey
            , Gen.vrf  = error "vrf used while generating transactions"
            , Gen.hot  = error "hot used while generating transactions"
            , Gen.hk   = error "hk used while generating transactions"
            }
          )
      , cnkiKeyPair = (mkKeyPair cnDelegateKey, mkKeyPair cnVestedDelagate, mkKeyPair cnStakingKey)
      }

genCoreNode ::
     forall c. OptimumCrypto c
  => SL.KESPeriod
  -> Gen (CoreNode c)
genCoreNode startKESPeriod = do
    genKey <- genKeyDSIGN <$> genSeed (seedSizeDSIGN (Proxy @(DSIGN c)))
    delKey <- genKeyDSIGN <$> genSeed (seedSizeDSIGN (Proxy @(DSIGN c)))
    vesKey <- genKeyDSIGN <$> genSeed (seedSizeDSIGN (Proxy @(DSIGN c)))
    vsdKey <- genKeyDSIGN <$> genSeed (seedSizeDSIGN (Proxy @(DSIGN c)))
    stkKey <- genKeyDSIGN <$> genSeed (seedSizeDSIGN (Proxy @(DSIGN c)))
    vrfKey <- genKeyVRF   <$> genSeed (seedSizeVRF   (Proxy @(VRF   c)))
    kesKey <- genKeyKES   <$> genSeed (seedSizeKES   (Proxy @(KES   c)))
    let kesPub = deriveVerKeyKES kesKey
        sigma  = Bcc.Ledger.Keys.signedDSIGN
          @c
          delKey
          vsdKey
          (SL.OCertSignable kesPub certificateIssueNumber startKESPeriod)
    let ocert = SL.OCert {
            ocertVkHot     = kesPub
          , ocertN         = certificateIssueNumber
          , ocertKESPeriod = startKESPeriod
          , ocertSigma     = sigma
          }
    return CoreNode {
        cnGenesisKey  = genKey
      , cnDelegateKey = delKey
      , cnVestKey     = vesKey
      , cnVestedDelagateKey = vsdKey
      , cnStakingKey  = stkKey
      , cnVRF         = vrfKey
      , cnKES         = kesKey
      , cnOCert       = ocert
      }
  where
    certificateIssueNumber = 0

    genBytes :: Integral a => a -> Gen BS.ByteString
    genBytes nbBytes = BS.pack <$> vectorOf (fromIntegral nbBytes) arbitrary

    genSeed :: Integral a => a -> Gen Bcc.Crypto.Seed
    genSeed = fmap mkSeedFromBytes . genBytes

mkLeaderCredentials :: OptimumCrypto c => CoreNode c -> TOptimumLeaderCredentials c
mkLeaderCredentials CoreNode { cnDelegateKey, cnVestedDelagateKey cnVRF, cnKES, cnOCert } =
    TOptimumLeaderCredentials {
        toptimumLeaderCredentialsInitSignKey = cnKES
      , toptimumLeaderCredentialsCanBeLeader = TOptimumCanBeLeader {
          toptimumCanBeLeaderOpCert     = cnOCert
        , toptimumCanBeLeaderColdVerKey = SL.VKey $ deriveVerKeyDSIGN cnDelegateKey
        , toptimumCanBeLeaderCold
        , toptimumCanBeLeaderSignKeyVRF = cnVRF
        }
      , toptimumLeaderCredentialsLabel       = "ThreadNet"
      }

{-------------------------------------------------------------------------------
  KES configuration
-------------------------------------------------------------------------------}

-- | Currently @'maxEvolutions' * 'slotsPerEvolution'@ is the max number of
-- slots the test can run without needing new ocerts.
--
-- TODO This limitation may be lifted by PR #2107, see
-- <https://github.com/The-Blockchain-Company/shardagnostic-network/issues/2107>.
data KesConfig = KesConfig
  { maxEvolutions     :: Word64
  , slotsPerEvolution :: Word64
  }

-- | A 'KesConfig' that will not require more evolutions than this test's crypto
-- allows.
mkKesConfig
  :: forall proxy c. Crypto c
  => proxy c -> NumSlots -> KesConfig
mkKesConfig _ (NumSlots t) = KesConfig
    { maxEvolutions
    , slotsPerEvolution = divCeiling t maxEvolutions
    }
  where
    maxEvolutions = fromIntegral $ totalPeriodsKES (Proxy @(KES c))

    -- | Like 'div', but rounds-up.
    divCeiling :: Integral a => a -> a -> a
    divCeiling n d = q + min 1 r
      where
        (q, r) = quotRem n d

{-------------------------------------------------------------------------------
  TOptimum node configuration
-------------------------------------------------------------------------------}

-- | The epoch size, given @k@ and @f@.
--
-- INVARIANT: @10 * k / f@ must be a whole number.
mkEpochSize :: SecurityParam -> Rational -> EpochSize
mkEpochSize (SecurityParam k) f =
    if r /= 0 then error "10 * k / f must be a whole number" else
    EpochSize q
  where
    n = numerator   f
    d = denominator f

    (q, r) = quotRem (10 * k * fromInteger d) (fromInteger n)

-- | Note: a KES algorithm supports a particular max number of KES evolutions,
-- but we can configure a potentially lower maximum for the ledger, that's why
-- we take it as an argument.
mkGenesisConfig
  :: forall era. OptimumCrypto (EraCrypto era)
  => ProtVer   -- ^ Initial protocol version
  -> SecurityParam
  -> Rational  -- ^ Initial active slot coefficient
  -> DecentralizationParam
  -> Word64 -- ^ Initial vest multiple
  -> Word64
     -- ^ Max Entropic supply, must be >= #coreNodes * initialEntropicPerCoreNode
  -> SlotLength
  -> KesConfig
  -> [CoreNode (EraCrypto era)]
  -> SophieGenesis era
mkGenesisConfig pVer k f d maxEntropicSupply slotLength kesCfg coreNodes =
    assertWithMsg checkMaxEntropicSupply $
    SophieGenesis {
      -- Matches the start of the ThreadNet tests
      sgSystemStart           = dawnOfTime
    , sgNetworkMagic          = 0
    , sgNetworkId             = networkId
    , sgVestMultiple	      = vestMultiple
    , sgVestedDelegs          = coreNodesToGenesisMapping
    , sgActiveSlotsCoeff      = unsafeBoundRational f
    , sgSecurityParam         = maxRollbacks k
    , sgEpochLength           = mkEpochSize k f
    , sgSlotsPerKESPeriod     = slotsPerEvolution kesCfg
    , sgMaxKESEvolutions      = maxEvolutions     kesCfg
    , sgSlotLength            = getSlotLength slotLength
    , sgUpdateQuorum          = quorum
    , sgMaxEntropicSupply     = maxEntropicSupply
    , sgProtocolParams        = pparams
    , sgGenDelegs             = coreNodesToGenesisMapping
    , sgInitialFunds          = initialFunds
    , sgStaking               = initialStake
    }
  where
    checkMaxEntropicSupply :: Either String ()
    checkMaxEntropicSupply
      | maxEntropicSupply >=
        fromIntegral (length coreNodes) * initialEntropicPerCoreNode
      = return ()
      | otherwise
      = throwError $ unwords [
            "Entropic supply ="
          , show maxEntropicSupply
          , "but must be at least"
          , show (fromIntegral (length coreNodes) * initialEntropicPerCoreNode)
          ]

    quorum :: Word64
    quorum = nbCoreNodes `min` ((nbCoreNodes `div` 2) + 1)
      where
        nbCoreNodes = fromIntegral (length coreNodes)

    pparams :: SL.PParams era
    pparams = SL.emptyPParams
      { SL._d               =
          unsafeBoundRational (decentralizationParamToRational d)
      , SL._maxBBSize       = 10000 -- TODO
      , SL._maxBHSize       = 1000 -- TODO
      , SL._protocolVersion = pVer
      }

    coreNodesToGenesisMapping ::
         Map (SL.KeyHash 'SL.Genesis (EraCrypto era)) (SL.GenDelegPair (EraCrypto era)) (SL.
    coreNodesToGenesisMapping  = Map.fromList
      [ let
          gkh :: SL.KeyHash 'SL.Genesis (EraCrypto era)
          gkh = SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnGenesisKey

          gdpair :: SL.GenDelegPair (EraCrypto era)
          gdpair = SL.GenDelegPair
              (SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnDelegateKey)
              (SL.hashVerKeyVRF $ deriveVerKeyVRF cnVRF)

        in (gkh, gdpair)
      | CoreNode { cnGenesisKey, cnDelegateKey, cnVRF } <- coreNodes
      ]

    initialFunds :: Map (SL.Addr (EraCrypto era)) SL.Coin
    initialFunds = Map.fromList
      [ (addr, coin)
      | CoreNode { cnDelegateKey, cnStakingKey } <- coreNodes
      , let addr = SL.Addr networkId
                           (mkCredential cnDelegateKey)
                           (SL.StakeRefBase (mkCredential cnStakingKey))
            coin = SL.Coin $ fromIntegral initialEntropicPerCoreNode
      ]

    -- In this initial stake, each core node delegates its stake to itself.
    initialStake :: SophieGenesisStaking (EraCrypto era)
    initialStake = SophieGenesisStaking
      { sgsPools = Map.fromList
          [ (pk, pp)
          | pp@(SL.PoolParams { _poolId = pk }) <- Map.elems coreNodeToPoolMapping
          ]
        -- The staking key maps to the key hash of the pool, which is set to the
        -- "delegate key" in order that nodes may issue blocks both as delegates
        -- and as stake pools.
      , sgsStake = Map.fromList
          [ ( SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnStakingKey
            , SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnDelegateKey
            )
          | CoreNode {cnDelegateKey, cnStakingKey} <- coreNodes
          ]
      }
      where
        coreNodeToPoolMapping ::
             Map (SL.KeyHash 'SL.StakePool (EraCrypto era)) (SL.PoolParams (EraCrypto era))
        coreNodeToPoolMapping = Map.fromList [
              ( SL.hashKey . SL.VKey . deriveVerKeyDSIGN $ cnStakingKey
              , SL.PoolParams
                { SL._poolId = poolHash
                , SL._poolVrf = vrfHash
                  -- Each core node pledges its full stake to the pool.
                , SL._poolPledge = SL.Coin $ fromIntegral initialEntropicPerCoreNode
                , SL._poolCost = SL.Coin 1
                , SL._poolMargin = minBound
                  -- Reward accounts live in a separate "namespace" to other
                  -- accounts, so it should be fine to use the same address.
                , SL._poolRAcnt = SL.RewardAcnt networkId $ mkCredential cnDelegateKey
                , SL._poolOwners = Set.singleton poolOwnerHash
                , SL._poolRelays = Seq.empty
                , SL._poolMD = SL.SNothing
                }
              )
            | CoreNode { cnDelegateKey, cnStakingKey, cnVRF } <- coreNodes
              -- The pool and owner hashes are derived from the same key, but
              -- use different hashing schemes
            , let poolHash = SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnDelegateKey
            , let poolOwnerHash = SL.hashKey . SL.VKey $ deriveVerKeyDSIGN cnDelegateKey
            , let vrfHash = SL.hashVerKeyVRF $ deriveVerKeyVRF cnVRF
            ]

mkProtocolSophie ::
     forall m c. (IOLike m, SophieBasedEra (SophieEra c))
  => SophieGenesis (SophieEra c)
  -> SL.Nonce
  -> ProtVer
  -> CoreNode c
  -> ProtocolInfo m (SophieBlock (SophieEra c))
mkProtocolSophie genesis initialNonce protVer coreNode =
    protocolInfoSophie
      ProtocolParamsSophieBased {
          sophieBasedGenesis           = genesis
        , sophieBasedInitialNonce      = initialNonce
        , sophieBasedLeaderCredentials = [mkLeaderCredentials coreNode]
        }
      ProtocolParamsSophie {
          sophieProtVer                = protVer
        , sophieMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
{-------------------------------------------------------------------------------
  Necessary transactions for updating the 'DecentralizationParam'
-------------------------------------------------------------------------------}

incrementSentryProtVer :: SL.ProtVer -> SL.ProtVer
incrementSentryProtVer (SL.ProtVer major sentry) = SL.ProtVer major (succ sentry)

mkSetDecentralizationParamTxs ::
     forall c. SophieBasedEra (SophieEra c)
  => [CoreNode c]
  -> ProtVer   -- ^ The proposed protocol version
  -> SlotNo   -- ^ The TTL
  -> DecentralizationParam   -- ^ The new value
  -> [GenTx (SophieBlock (SophieEra c))]
mkSetDecentralizationParamTxs coreNodes pVer ttl dNew =
    (:[]) $
    mkSophieTx $
    SL.Tx
      { body            = body
      , wits            = witnessSet
      , auxiliaryData   = SL.SNothing
      }
  where
    -- The funds touched by this transaction assume it's the first transaction
    -- executed.
    scheduledEpoch :: EpochNo
    scheduledEpoch = EpochNo 0

    witnessSet :: SL.WitnessSet (SophieEra c)
    witnessSet = SL.WitnessSet signatures mempty mempty

    -- Every node signs the transaction body, since it includes a " vote " from
    -- every node.
    signatures :: Set (SL.WitVKey 'SL.Witness c)
    signatures =
        SL.makeWitnessesVKey
          (hashAnnotated body)
          [ SL.KeyPair (SL.VKey vk) sk
          | cn <- coreNodes
          , let sk = cnDelegateKey cn
          , let vk = deriveVerKeyDSIGN sk
          ]

    -- Nothing but the parameter update and the obligatory touching of an
    -- input.
    body :: SL.TxBody (SophieEra c)
    body = SL.TxBody
      { _certs    = Seq.empty
      , _inputs   = Set.singleton (fst touchCoins)
      , _mdHash   = SL.SNothing
      , _outputs  = Seq.singleton (snd touchCoins)
      , _ttl      = ttl
      , _txfee    = SL.Coin 0
      , _txUpdate = SL.SJust update
      , _wdrls    = SL.Wdrl Map.empty
      }

    -- Every Sophie transaction requires one input.
    --
    -- We use the input of the first node, but we just put it all right back.
    --
    -- ASSUMPTION: This transaction runs in the first slot.
    touchCoins :: (SL.TxIn c, SL.TxOut (SophieEra c))
    touchCoins = case coreNodes of
        []   -> error "no nodes!"
        cn:_ ->
            ( SL.initialFundsPseudoTxIn addr
            , SL.TxOut addr coin
            )
          where
            addr = SL.Addr networkId
                (mkCredential (cnDelegateKey cn))
                (SL.StakeRefBase (mkCredential (cnStakingKey cn)))
            coin = SL.Coin $ fromIntegral initialEntropicPerCoreNode

    -- One replicant of the parameter update per each node.
    update :: SL.Update (SophieEra c)
    update =
        flip SL.Update scheduledEpoch $ SL.ProposedPPUpdates $
        Map.fromList $
        [ ( SL.hashKey $ SL.VKey $ deriveVerKeyDSIGN $ cnGenesisKey cn
          , SL.emptyPParamsUpdate
              { SL._d =
                  maybeToStrictMaybe $
                  boundRational $
                  decentralizationParamToRational dNew
              , SL._protocolVersion =
                  SL.SJust pVer
              }
          )
        | cn <- coreNodes
        ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

initialEntropicPerCoreNode :: Word64
initialEntropicPerCoreNode = 1000000

mkCredential :: OptimumCrypto c => SL.SignKeyDSIGN c -> SL.Credential r c
mkCredential = SL.KeyHashObj . mkKeyHash

mkKeyHash :: OptimumCrypto c => SL.SignKeyDSIGN c -> SL.KeyHash r c
mkKeyHash = SL.hashKey . mkVerKey

mkVerKey :: OptimumCrypto c => SL.SignKeyDSIGN c -> SL.VKey r c
mkVerKey = SL.VKey . deriveVerKeyDSIGN

mkKeyPair :: OptimumCrypto c => SL.SignKeyDSIGN c -> SL.KeyPair r c
mkKeyPair sk = SL.KeyPair { vKey = mkVerKey sk, sKey = sk }

mkKeyHashVrf :: (HashAlgorithm h, VRFAlgorithm vrf)
             => SignKeyVRF vrf
             -> Hash h (VerKeyVRF vrf)
mkKeyHashVrf = SL.hashVerKeyVRF . deriveVerKeyVRF

networkId :: SL.Network
networkId = SL.Testnet

{-------------------------------------------------------------------------------
  Temporary Workaround
-------------------------------------------------------------------------------}

-- | TODO This is a copy-paste-edit of 'mkSetDecentralizationParamTxs'
--
-- Our current plan is to replace all of this infrastructure with the ThreadNet
-- rewrite; so we're minimizing the work and maintenance here for now.
mkMASetDecentralizationParamTxs ::
     forall era.
     ( SophieBasedEra era
     , Core.Tx era ~ SL.Tx era
     , Core.TxBody era ~ MA.TxBody era
     , Core.PParams era ~ SL.PParams era
     , Core.PParamsDelta era ~ SL.PParams' SL.StrictMaybe era
     , Core.Witnesses era ~ SL.WitnessSet era
     )
  => [CoreNode (Core.Crypto era)]
  -> ProtVer   -- ^ The proposed protocol version
  -> SlotNo   -- ^ The TTL
  -> DecentralizationParam   -- ^ The new value
  -> [GenTx (SophieBlock era)]
mkMASetDecentralizationParamTxs coreNodes pVer ttl dNew =
    (:[]) $
    mkSophieTx $
    SL.Tx
      { body          = body
      , wits          = witnessSet
      , auxiliaryData = SL.SNothing
      }
  where
    -- The funds touched by this transaction assume it's the first transaction
    -- executed.
    scheduledEpoch :: EpochNo
    scheduledEpoch = EpochNo 0

    witnessSet :: SL.WitnessSet era
    witnessSet = SL.WitnessSet signatures mempty mempty

    -- Every node signs the transaction body, since it includes a " vote " from
    -- every node.
    signatures :: Set (SL.WitVKey 'SL.Witness (Core.Crypto era))
    signatures =
        SL.makeWitnessesVKey
          (eraIndTxBodyHash' body)
          [ SL.KeyPair (SL.VKey vk) sk
          | cn <- coreNodes
          , let sk = cnDelegateKey cn
          , let vk = deriveVerKeyDSIGN sk
          ]

    -- Nothing but the parameter update and the obligatory touching of an
    -- input.
    body :: MA.TxBody era
    body = MA.TxBody
        inputs
        outputs
        certs
        wdrls
        txfee
        vldt
        update'
        adHash
        mint
      where
        inputs   = Set.singleton (fst touchCoins)
        outputs  = Seq.singleton (snd touchCoins)
        certs    = Seq.empty
        wdrls    = SL.Wdrl Map.empty
        txfee    = SL.Coin 0
        vldt     = MA.ValidityInterval {
              invalidBefore    = SL.SNothing
            , invalidHereafter = SL.SJust ttl
            }
        update'  = SL.SJust update
        adHash   = SL.SNothing
        mint     = SL.inject $ SL.Coin 0

    -- Every Sophie transaction requires one input.
    --
    -- We use the input of the first node, but we just put it all right back.
    --
    -- ASSUMPTION: This transaction runs in the first slot.
    touchCoins :: (SL.TxIn (Core.Crypto era), SL.TxOut era)
    touchCoins = case coreNodes of
        []   -> error "no nodes!"
        cn:_ ->
            ( SL.initialFundsPseudoTxIn addr
            , SL.TxOut addr coin
            )
          where
            addr = SL.Addr networkId
                (mkCredential (cnDelegateKey cn))
                (SL.StakeRefBase (mkCredential (cnStakingKey cn)))
            coin = SL.inject $ SL.Coin $ fromIntegral initialEntropicPerCoreNode

    -- One replicant of the parameter update per each node.
    update :: SL.Update era
    update =
        flip SL.Update scheduledEpoch $ SL.ProposedPPUpdates $
        Map.fromList $
        [ ( SL.hashKey $ SL.VKey $ deriveVerKeyDSIGN $ cnGenesisKey cn
          , SL.emptyPParamsUpdate
              { SL._d =
                  maybeToStrictMaybe $
                  boundRational $
                  decentralizationParamToRational dNew
              , SL._protocolVersion =
                  SL.SJust pVer
              }
          )
        | cn <- coreNodes
        ]

eraIndTxBodyHash' ::
     forall crypto body.
     ( HashAlgorithm (Bcc.Ledger.Crypto.HASH crypto)
     , HashAnnotated body EraIndependentTxBody crypto
     )
  => body
  -> SafeHash
       crypto
       EraIndependentTxBody
eraIndTxBodyHash' = coerce . hashAnnotated
