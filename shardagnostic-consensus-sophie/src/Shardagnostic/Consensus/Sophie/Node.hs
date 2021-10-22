{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE UndecidableSuperClasses  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Shardagnostic.Consensus.Sophie.Node (
    MaxMajorProtVer (..)
  , ProtocolParamsEvie (..)
  , ProtocolParamsAurum (..)
  , ProtocolParamsJen (..)
  , ProtocolParamsSophie (..)
  , ProtocolParamsSophieBased (..)
  , SL.Nonce (..)
  , SL.ProtVer (..)
  , SL.SophieGenesis (..)
  , SL.SophieGenesisStaking (..)
  , SL.emptyGenesisStaking
  , TOptimumLeaderCredentials (..)
  , protocolClientInfoSophie
  , protocolInfoSophie
  , protocolInfoSophieBased
  , registerGenesisStaking
  , registerInitialFunds
  , sophieBlockForging
  , sophieSharedBlockForging
  , toptimumBlockIssuerVKey
  , validateGenesis
  ) where

import           Control.Monad.Except (Except)
import           Data.Bifunctor (first)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Stack (HasCallStack)

import qualified Bcc.Crypto.VRF as VRF
import           Bcc.Slotting.EpochInfo
import           Bcc.Slotting.Time (mkSlotLength)

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Config
import           Shardagnostic.Consensus.Config.SupportsNode
import qualified Shardagnostic.Consensus.HardFork.History as History
import           Shardagnostic.Consensus.HeaderValidation
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.Ledger.Extended
import           Shardagnostic.Consensus.Mempool.TxLimits (TxLimits)
import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits
import           Shardagnostic.Consensus.Node.InitStorage
import           Shardagnostic.Consensus.Node.ProtocolInfo
import           Shardagnostic.Consensus.Node.Run
import           Shardagnostic.Consensus.Protocol.Abstract
import           Shardagnostic.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import           Shardagnostic.Consensus.Util.Assert
import           Shardagnostic.Consensus.Util.IOLike

import qualified Bcc.Ledger.Era as Core
import qualified Bcc.Ledger.Sophie.Constraints as SL (makeTxOut)
import           Bcc.Ledger.Val (coin, inject, (<->))
import qualified Bcc.Protocol.TOptimum.OCert as Absolute (KESPeriod (..))
import qualified Sophie.Spec.Ledger.API as SL
import qualified Sophie.Spec.Ledger.LedgerState as SL (stakeDistr)

import           Shardagnostic.Consensus.Sophie.Eras
import           Shardagnostic.Consensus.Sophie.Ledger
import           Shardagnostic.Consensus.Sophie.Ledger.Inspect ()
import           Shardagnostic.Consensus.Sophie.Ledger.NetworkProtocolVersion ()
import           Shardagnostic.Consensus.Sophie.Node.Serialisation ()
import           Shardagnostic.Consensus.Sophie.Protocol
import           Shardagnostic.Consensus.Sophie.Protocol.HotKey (HotKey)
import qualified Shardagnostic.Consensus.Sophie.Protocol.HotKey as HotKey

{-------------------------------------------------------------------------------
  Credentials
-------------------------------------------------------------------------------}

data TOptimumLeaderCredentials c = TOptimumLeaderCredentials {
      -- | The unevolved signing KES key (at evolution 0).
      --
      -- Note that this is not inside 'TOptimumCanBeLeader' since it gets evolved
      -- automatically, whereas 'TOptimumCanBeLeader' does not change.
      toptimumLeaderCredentialsInitSignKey :: SL.SignKeyKES c
    , toptimumLeaderCredentialsCanBeLeader :: TOptimumCanBeLeader c
      -- | Identifier for this set of credentials.
      --
      -- Useful when the node is running with multiple sets of credentials.
    , toptimumLeaderCredentialsLabel       :: Text
    }

toptimumBlockIssuerVKey ::
     TOptimumLeaderCredentials c -> SL.VKey 'SL.BlockIssuer c
toptimumBlockIssuerVKey =
    toptimumCanBeLeaderColdVerKey . toptimumLeaderCredentialsCanBeLeader

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

type instance CannotForge (SophieBlock era) = TOptimumCannotForge (EraCrypto era)

type instance ForgeStateInfo (SophieBlock era) = HotKey.KESInfo

type instance ForgeStateUpdateError (SophieBlock era) = HotKey.KESEvolutionError

-- | Create a 'BlockForging' record for a single era.
--
-- In case the same credentials should be shared across multiple Sophie-based
-- eras, use 'sophieSharedBlockForging'.
sophieBlockForging ::
     forall m era. (SophieBasedEra era, TxLimits (SophieBlock era), IOLike m)
  => TOptimumParams
  -> TxLimits.Overrides (SophieBlock era)
  -> TOptimumLeaderCredentials (EraCrypto era)
  -> m (BlockForging m (SophieBlock era))
sophieBlockForging toptimumParams maxTxCapacityOverrides credentials =
      fmap aux
    $ sophieSharedBlockForging
        (Proxy @'[era])
        toptimumParams
        credentials
        (Comp maxTxCapacityOverrides :* Nil)
  where
    aux ::
         NP (BlockForging m :.: SophieBlock) '[era]
      -> BlockForging m (SophieBlock era)
    aux = unComp . hd

-- | Needed in 'sophieSharedBlockForging' because we can't partially apply
-- equality constraints.
class    (SophieBasedEra era, TxLimits (SophieBlock era), EraCrypto era ~ c) => SophieEraWithCrypto c era
instance (SophieBasedEra era, TxLimits (SophieBlock era), EraCrypto era ~ c) => SophieEraWithCrypto c era

-- | Create a 'BlockForging' record for each of the given Sophie-based eras,
-- safely sharing the same set of credentials for all of them.
--
-- The name of the era (separated by a @_@) will be appended to each
-- 'forgeLabel'.
sophieSharedBlockForging ::
     forall m c eras.
     ( OptimumCrypto c
     , All (SophieEraWithCrypto c) eras
     , IOLike m
     )
  => Proxy eras
  -> TOptimumParams
  -> TOptimumLeaderCredentials c
  -> NP    (TxLimits.Overrides :.: SophieBlock) eras
  -> m (NP (BlockForging m     :.: SophieBlock) eras)
sophieSharedBlockForging
                    _
                    TOptimumParams {..}
                    TOptimumLeaderCredentials {
                        toptimumLeaderCredentialsInitSignKey = initSignKey
                      , toptimumLeaderCredentialsCanBeLeader = canBeLeader
                      , toptimumLeaderCredentialsLabel       = label
                      }
                    maxTxCapacityOverridess = do
    hotKey <- HotKey.mkHotKey @m @c initSignKey startPeriod toptimumMaxKESEvo
    return $
      hcmap
        (Proxy @(SophieEraWithCrypto c))
        (aux hotKey)
        maxTxCapacityOverridess
  where
    aux ::
         forall era. SophieEraWithCrypto c era
      => HotKey c m
      -> (TxLimits.Overrides :.: SophieBlock) era
      -> (BlockForging m     :.: SophieBlock) era
    aux hotKey (Comp maxTxCapacityOverrides) = Comp $ BlockForging {
          forgeLabel       = label <> "_" <> sophieBasedEraName (Proxy @era)
        , canBeLeader      = canBeLeader
        , updateForgeState = \_ curSlot _ ->
                                 forgeStateUpdateInfoFromUpdateInfo <$>
                                   HotKey.evolve hotKey (slotToPeriod curSlot)
        , checkCanForge    = \cfg curSlot _tickedChainDepState ->
                                 toptimumCheckCanForge
                                   (configConsensus cfg)
                                   forgingVRFHash
                                   curSlot
        , forgeBlock       = \cfg ->
            forgeSophieBlock
              hotKey
              canBeLeader
              cfg
              maxTxCapacityOverrides
        }

    forgingVRFHash :: SL.Hash c (SL.VerKeyVRF c)
    forgingVRFHash =
          SL.hashVerKeyVRF
        . VRF.deriveVerKeyVRF
        . toptimumCanBeLeaderSignKeyVRF
        $ canBeLeader

    startPeriod :: Absolute.KESPeriod
    startPeriod = SL.ocertKESPeriod $ toptimumCanBeLeaderOpCert canBeLeader

    slotToPeriod :: SlotNo -> Absolute.KESPeriod
    slotToPeriod (SlotNo slot) =
        SL.KESPeriod $ fromIntegral $ slot `div` toptimumSlotsPerKESPeriod

{-------------------------------------------------------------------------------
  ProtocolInfo
-------------------------------------------------------------------------------}

-- | Check the validity of the genesis config. To be used in conjunction with
-- 'assertWithMsg'.
validateGenesis ::
     SophieBasedEra era
  => SL.SophieGenesis era -> Either String ()
validateGenesis = first errsToString . SL.validateGenesis
  where
    errsToString :: [SL.ValidationErr] -> String
    errsToString errs =
        Text.unpack $ Text.unlines
          ("Invalid genesis config:" : map SL.describeValidationErr errs)

-- | Parameters common to all Sophie-based ledgers.
--
-- When running a chain with multiple Sophie-based eras, in addition to the
-- per-era protocol parameters, one value of 'ProtocolParamsSophieBased' will
-- be needed, which is shared among all Sophie-based eras.
--
-- The @era@ parameter determines from which era the genesis config will be
-- used.
data ProtocolParamsSophieBased era = ProtocolParamsSophieBased {
      sophieBasedGenesis           :: SL.SophieGenesis era
      -- | The initial nonce, typically derived from the hash of Genesis
      -- config JSON file.
      --
      -- WARNING: chains using different values of this parameter will be
      -- mutually incompatible.
    , sophieBasedInitialNonce      :: SL.Nonce
    , sophieBasedLeaderCredentials :: [TOptimumLeaderCredentials (EraCrypto era)]
    }

-- | Parameters needed to run Sophie
data ProtocolParamsSophie c = ProtocolParamsSophie {
      sophieProtVer                :: SL.ProtVer
    , sophieMaxTxCapacityOverrides :: TxLimits.Overrides (SophieBlock (SophieEra c))
    }

-- | Parameters needed to run Evie
data ProtocolParamsEvie c = ProtocolParamsEvie {
      evieProtVer                :: SL.ProtVer
    , evieMaxTxCapacityOverrides :: TxLimits.Overrides (SophieBlock (EvieEra c))
    }

-- | Parameters needed to run Jen
data ProtocolParamsJen c = ProtocolParamsJen {
      jenProtVer                :: SL.ProtVer
    , jenMaxTxCapacityOverrides :: TxLimits.Overrides (SophieBlock (JenEra c))
    }

-- | Parameters needed to run Aurum
data ProtocolParamsAurum c = ProtocolParamsAurum {
      aurumProtVer                :: SL.ProtVer
    , aurumMaxTxCapacityOverrides :: TxLimits.Overrides (SophieBlock (AurumEra c))
    }

protocolInfoSophie ::
     forall m c. (IOLike m, SophieBasedEra (SophieEra c), TxLimits (SophieBlock (SophieEra c)))
  => ProtocolParamsSophieBased (SophieEra c)
  -> ProtocolParamsSophie c
  -> ProtocolInfo m (SophieBlock (SophieEra c))
protocolInfoSophie protocolParamsSophieBased
                    ProtocolParamsSophie {
                        sophieProtVer                = protVer
                      , sophieMaxTxCapacityOverrides = maxTxCapacityOverrides
                      } =
    protocolInfoSophieBased
      protocolParamsSophieBased
      ()  -- trivial translation context
      protVer
      maxTxCapacityOverrides

protocolInfoSophieBased ::
     forall m era. (IOLike m, SophieBasedEra era, TxLimits (SophieBlock era))
  => ProtocolParamsSophieBased era
  -> Core.TranslationContext era
  -> SL.ProtVer
  -> TxLimits.Overrides (SophieBlock era)
  -> ProtocolInfo m     (SophieBlock era)
protocolInfoSophieBased ProtocolParamsSophieBased {
                             sophieBasedGenesis           = genesis
                           , sophieBasedInitialNonce      = initialNonce
                           , sophieBasedLeaderCredentials = credentialss
                           }
                         transCtxt
                         protVer
                         maxTxCapacityOverrides =
    assertWithMsg (validateGenesis genesis) $
    ProtocolInfo {
        pInfoConfig       = topLevelConfig
      , pInfoInitLedger   = initExtLedgerState
      , pInfoBlockForging =
          traverse
            (sophieBlockForging toptimumParams maxTxCapacityOverrides)
            credentialss
      }
  where

    -- | Currently for all existing eras in ledger-specs (Sophie, Evie, Jen
    -- and Aurum) it happens to be the case that AdditionalGenesisConfig and
    -- TranslationContext are instantiated to the same type.
    -- We take advantage of this fact below to simplify our code, but we are
    -- aware that this might change in future (for new eras), breaking this
    -- code.
    --
    -- see type equality constraint in
    -- Shardagnostic.Consensus.Sophie.Eras.SophieBasedEra
    additionalGenesisConfig :: SL.AdditionalGenesisConfig era
    additionalGenesisConfig = transCtxt

    maxMajorProtVer :: MaxMajorProtVer
    maxMajorProtVer = MaxMajorProtVer $ SL.pvMajor protVer

    topLevelConfig :: TopLevelConfig (SophieBlock era)
    topLevelConfig = TopLevelConfig {
        topLevelConfigProtocol = consensusConfig
      , topLevelConfigLedger   = ledgerConfig
      , topLevelConfigBlock    = blockConfig
      , topLevelConfigCodec    = SophieCodecConfig
      , topLevelConfigStorage  = storageConfig
      }

    consensusConfig :: ConsensusConfig (BlockProtocol (SophieBlock era))
    consensusConfig = TOptimumConfig {
        toptimumParams
      , toptimumEpochInfo = epochInfo
      }

    ledgerConfig :: LedgerConfig (SophieBlock era)
    ledgerConfig = mkSophieLedgerConfig genesis transCtxt epochInfo maxMajorProtVer

    epochInfo :: EpochInfo (Except History.PastHorizonException)
    epochInfo =
        fixedEpochInfo
          (SL.sgEpochLength genesis)
          (mkSlotLength $ SL.sgSlotLength genesis)

    toptimumParams :: TOptimumParams
    toptimumParams = mkTOptimumParams maxMajorProtVer initialNonce genesis

    blockConfig :: BlockConfig (SophieBlock era)
    blockConfig =
        mkSophieBlockConfig
          protVer
          genesis
          (toptimumBlockIssuerVKey <$> credentialss)

    storageConfig :: StorageConfig (SophieBlock era)
    storageConfig = SophieStorageConfig {
          sophieStorageConfigSlotsPerKESPeriod = toptimumSlotsPerKESPeriod toptimumParams
        , sophieStorageConfigSecurityParam     = toptimumSecurityParam     toptimumParams
        }

    initLedgerState :: LedgerState (SophieBlock era)
    initLedgerState = SophieLedgerState {
        sophieLedgerTip        = Origin
      , sophieLedgerState      =
          registerGenesisStaking (SL.sgStaking genesis) $
            SL.initialState genesis additionalGenesisConfig
      , sophieLedgerTransition = SophieTransitionInfo {sophieAfterVoting = 0}
      }

    initChainDepState :: TOptimumState (EraCrypto era)
    initChainDepState = TOptimumState Origin $
      SL.initialChainDepState initialNonce (SL.sgGenDelegs genesis)

    initExtLedgerState :: ExtLedgerState (SophieBlock era)
    initExtLedgerState = ExtLedgerState {
        ledgerState = initLedgerState
      , headerState = genesisHeaderState initChainDepState
      }

protocolClientInfoSophie :: ProtocolClientInfo (SophieBlock era)
protocolClientInfoSophie =
    ProtocolClientInfo {
      -- No particular codec configuration is needed for Sophie
      pClientInfoCodecConfig = SophieCodecConfig
    }

{-------------------------------------------------------------------------------
  ConfigSupportsNode instance
-------------------------------------------------------------------------------}

instance ConfigSupportsNode (SophieBlock era) where
  getSystemStart  = sophieSystemStart
  getNetworkMagic = sophieNetworkMagic

{-------------------------------------------------------------------------------
  NodeInitStorage instance
-------------------------------------------------------------------------------}

instance SophieBasedEra era => NodeInitStorage (SophieBlock era) where
  -- We fix the chunk size to @10k@ so that we have the same chunk size as
  -- Cole. Consequently, a Sophie net will have the same chunk size as the
  -- Cole-to-Sophie net with the same @k@.
  nodeImmutableDbChunkInfo =
        simpleChunkInfo
      . EpochSize
      . (* 10)
      . maxRollbacks
      . sophieStorageConfigSecurityParam

  nodeCheckIntegrity cfg =
      verifyBlockIntegrity (sophieStorageConfigSlotsPerKESPeriod cfg)

{-------------------------------------------------------------------------------
  RunNode instance
-------------------------------------------------------------------------------}

instance SophieBasedEra era => BlockSupportsMetrics (SophieBlock era) where
  isSelfIssued cfg hdr =
       case csvSelfIssued $ selectView cfg hdr of
         SelfIssued    -> IsSelfIssued
         NotSelfIssued -> IsNotSelfIssued

instance SophieBasedEra era => RunNode (SophieBlock era)

{-------------------------------------------------------------------------------
  Register genesis staking
-------------------------------------------------------------------------------}

-- | Register the initial staking information in the 'SL.NewEpochState'.
--
-- HERE BE DRAGONS! This function is intended to help in testing.
--
-- In production, the genesis should /not/ contain any initial staking.
--
-- Any existing staking information is overridden, but the UTxO is left
-- untouched.
--
-- TODO adapt and reuse @registerGenesisStaking@ from @bcc-ledger-specs@.
registerGenesisStaking ::
     forall era. SophieBasedEra era
  => SL.SophieGenesisStaking (EraCrypto era)
  -> SL.NewEpochState era
  -> SL.NewEpochState era
registerGenesisStaking staking nes = nes {
      SL.nesEs = epochState {
          SL.esLState = ledgerState {
          SL._delegationState = dpState {
              SL._dstate = dState'
            , SL._pstate = pState'
            }
        }
        , SL.esSnapshots = (SL.esSnapshots epochState) {
              SL._pstakeMark = initSnapShot
            }
        }

    -- Note that this is only applicable in the initial configuration where
    -- there is no existing stake distribution, since it would completely
    -- overwrite any such thing.
    , SL.nesPd = SL.calculatePoolDistr initSnapShot
    }
  where
    SL.SophieGenesisStaking { sgsPools, sgsStake } = staking
    SL.NewEpochState { nesEs = epochState } = nes
    ledgerState = SL.esLState epochState
    dpState = SL._delegationState ledgerState

    -- New delegation state. Since we're using base addresses, we only care
    -- about updating the '_delegations' field.
    --
    -- See STS DELEG for details
    dState' :: SL.DState (EraCrypto era)
    dState' = (SL._dstate dpState) {
          SL._rewards = Map.map (const $ SL.Coin 0)
                      . Map.mapKeys SL.KeyHashObj
                      $ sgsStake
        , SL._delegations = Map.mapKeys SL.KeyHashObj sgsStake
        }

    -- We consider pools as having been registered in slot 0
    -- See STS POOL for details
    pState' :: SL.PState (EraCrypto era)
    pState' = (SL._pstate dpState) {
          SL._pParams = sgsPools
        }

    -- The new stake distribution is made on the basis of a snapshot taken
    -- during the previous epoch. We create a "fake" snapshot in order to
    -- establish an initial stake distribution.
    initSnapShot :: SL.SnapShot (EraCrypto era)
    initSnapShot =
        SL.stakeDistr
          @era
          (SL._utxo (SL._utxoState ledgerState))
          dState'
          pState'

-- | Register the initial funds in the 'SL.NewEpochState'.
--
-- HERE BE DRAGONS! This function is intended to help in testing.
--
-- In production, the genesis should /not/ contain any initial funds.
--
-- The given funds are /added/ to the existing UTxO.
--
-- PRECONDITION: the given funds must not be part of the existing UTxO.
-- > forall (addr, _) in initialFunds.
-- >    Map.notElem (SL.initialFundsPseudoTxIn addr) existingUTxO
--
-- PROPERTY:
-- >    genesisUTxO genesis
-- > == <genesisUTxO'> (sgInitialFunds genesis)
-- > == <extractUTxO> (registerInitialFunds (sgInitialFunds genesis)
-- >                                        <empty NewEpochState>)
--
-- TODO move to @bcc-ledger-specs@.
registerInitialFunds ::
     forall era.
     ( SophieBasedEra era
     , HasCallStack
     )
  => Map (SL.Addr (EraCrypto era)) SL.Coin
  -> SL.NewEpochState era
  -> SL.NewEpochState era
registerInitialFunds initialFunds nes = nes {
      SL.nesEs = epochState {
          SL.esAccountState = accountState'
        , SL.esLState       = ledgerState'
        }
    }
  where
    epochState   = SL.nesEs          nes
    accountState = SL.esAccountState epochState
    ledgerState  = SL.esLState       epochState
    utxoState    = SL._utxoState     ledgerState
    utxo         = SL._utxo          utxoState
    reserves     = SL._reserves      accountState

    initialFundsUtxo :: SL.UTxO era
    initialFundsUtxo = SL.UTxO $ Map.fromList [
          (txIn, txOut)
        | (addr, amount) <- Map.toList initialFunds
        ,  let txIn  = SL.initialFundsPseudoTxIn addr
               txOut = SL.makeTxOut (Proxy @era) addr (inject amount)
        ]

    utxo' = mergeUtxoNoOverlap utxo initialFundsUtxo

    -- Update the reserves
    accountState' = accountState {
          SL._reserves = reserves <-> coin (SL.balance initialFundsUtxo)
        }

    ledgerState' = ledgerState {
          SL._utxoState = utxoState {
              SL._utxo = utxo'
            }
        }

    -- | Merge two UTxOs, throw an 'error' in case of overlap
    mergeUtxoNoOverlap ::
         HasCallStack
      => SL.UTxO era -> SL.UTxO era -> SL.UTxO era
    mergeUtxoNoOverlap (SL.UTxO m1) (SL.UTxO m2) = SL.UTxO $
        Map.unionWithKey
          (\k _ _ -> error $ "initial fund part of UTxO: " <> show k)
          m1
          m2
