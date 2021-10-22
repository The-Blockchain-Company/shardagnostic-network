{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Bcc (BccTxGenExtra (..)) where

import           Control.Exception (assert)

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)
import           Data.SOP.Strict
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set

import           Shardagnostic.Consensus.Block (SlotNo (..))
import           Shardagnostic.Consensus.Config
import           Shardagnostic.Consensus.HardFork.Combinator.Ledger
                     (tickedHardForkLedgerStatePerEra)
import           Shardagnostic.Consensus.HardFork.Combinator.State.Types
                     (currentState, getHardForkState)
import           Shardagnostic.Consensus.HardFork.Combinator.Util.Telescope as Tele
import           Shardagnostic.Consensus.Ledger.Basics (LedgerConfig, LedgerState,
                     applyChainTick)
import           Shardagnostic.Consensus.NodeId (CoreNodeId (..))

import           Bcc.Crypto (toVerification)
import qualified Bcc.Crypto.Signing as Cole

import qualified Bcc.Chain.Common as Cole
import           Bcc.Chain.Genesis (GeneratedSecrets (..))

import qualified Bcc.Ledger.Address as SL (BootstrapAddress (..))
import qualified Bcc.Ledger.Hashes as SL
import qualified Bcc.Ledger.SafeHash as SL
import           Bcc.Ledger.Val ((<->))
import qualified Sophie.Spec.Ledger.API as SL
import qualified Sophie.Spec.Ledger.Address.Bootstrap as SL
                     (makeBootstrapWitness)
import qualified Sophie.Spec.Ledger.Tx as SL (WitnessSetHKD (..))
import qualified Sophie.Spec.Ledger.UTxO as SL (makeWitnessVKey)

import           Shardagnostic.Consensus.Sophie.Ledger (GenTx, SophieBlock,
                     mkSophieTx)
import           Shardagnostic.Consensus.Sophie.Ledger.Ledger (Ticked,
                     tickedSophieLedgerState)

import           Shardagnostic.Consensus.Bcc
import           Shardagnostic.Consensus.Bcc.Block (BccEras, GenTx (..),
                     SophieEra)
import           Shardagnostic.Consensus.Bcc.Node (BccHardForkConstraints)

import qualified Test.ThreadNet.Infra.Sophie as Sophie
import           Test.ThreadNet.TxGen

data BccTxGenExtra c = BccTxGenExtra
  { ctgeColeGenesisKeys :: GeneratedSecrets
  , ctgeNetworkMagic     :: Cole.NetworkMagic
  , ctgeSophieCoreNodes :: [Sophie.CoreNode c]
  }

instance BccHardForkConstraints c => TxGen (BccBlock c) where

  type TxGenExtra (BccBlock c) = BccTxGenExtra c

  -- TODO also generate " typical " Cole and Sophie transactions
  testGenTxs (CoreNodeId i) _ncn curSlot cfg extra ls =
      pure $ maybeToList $ migrateUTxO migrationInfo curSlot lcfg ls
    where
      lcfg = topLevelConfigLedger cfg

      BccTxGenExtra
        { ctgeColeGenesisKeys
        , ctgeNetworkMagic
        , ctgeSophieCoreNodes
        } = extra

      GeneratedSecrets
        { gsRichSecrets
        } = ctgeColeGenesisKeys

      migrationInfo = MigrationInfo
        { coleMagic = ctgeNetworkMagic
        , coleSK
        , paymentSK
        , poolSK
        , stakingSK
        , vrfSK
        }

      coleSK :: Cole.SigningKey
      coleSK = gsRichSecrets !! fromIntegral i

      Sophie.CoreNode
        { Sophie.cnDelegateKey = paymentSK
        , Sophie.cnStakingKey  = stakingSK
        , Sophie.cnVRF         = vrfSK
        } = ctgeSophieCoreNodes !! fromIntegral i

      -- Reuse the payment key as the pool key, since it's an individual
      -- stake pool and the namespaces are separate.
      poolSK :: SL.SignKeyDSIGN c
      poolSK = paymentSK

-- | See 'migrateUTxO'
data MigrationInfo c = MigrationInfo
  { coleMagic :: Cole.NetworkMagic
    -- ^ Needed for creating a Cole address.
  , coleSK    :: Cole.SigningKey
    -- ^ The core node's Cole secret.
  , paymentSK  :: SL.SignKeyDSIGN c
  , poolSK     :: SL.SignKeyDSIGN c
  , stakingSK  :: SL.SignKeyDSIGN c
  , vrfSK      :: SL.SignKeyVRF   c
    -- ^ To be re-used by the individual pool.
  }

-- | Convert a core node's utxo from Cole to an active Sophie stake pool.
--
-- Returns a transaction that registers a staking key, registers an individual
-- stake pool, delegates that stake key to that stake pool, and transfers all
-- utxo from the Cole 'coleAddr' to the Sophie address corresponding to the
-- pair of 'paymentSK' and 'stakingSK'.
--
-- It returns 'Nothing' if the core node does not have any utxo in its
-- 'coleAddr' (eg if this transaction has already been applied).
migrateUTxO ::
     forall c. BccHardForkConstraints c
  => MigrationInfo c
  -> SlotNo
  -> LedgerConfig (BccBlock c)
  -> LedgerState (BccBlock c)
  -> Maybe (GenTx (BccBlock c))
migrateUTxO migrationInfo curSlot lcfg lst
    | Just utxo <- mbUTxO =

    let picked :: Map (SL.TxIn c) (SL.TxOut (SophieEra c))
        picked =
            Map.filter pick $ SL.unUTxO utxo
          where
            pick (SL.TxOut addr _) =
                addr == SL.AddrBootstrap (SL.BootstrapAddress coleAddr)

        -- Total held by 'coleAddr'
        pickedCoin :: SL.Coin
        pickedCoin = foldMap (\(SL.TxOut _ coin) -> coin) picked

        -- NOTE: The Bcc ThreadNet tests use the
        -- shardagnostic-consensus-sophie-test infra's genesis config, which sets
        -- relevant protocol params to 0.
        fee, deposits, spentCoin :: SL.Coin
        fee       = SL.Coin  0
        deposits  = SL.Coin 0
        spentCoin = deposits <> fee

        unspentCoin :: SL.Coin
        unspentCoin =
            assert (pickedCoin > spentCoin) $
            pickedCoin <-> spentCoin

        body :: SL.TxBody (SophieEra c)
        body = SL.TxBody
          { SL._certs    = StrictSeq.fromList $
              [ SL.DCertDeleg $ SL.RegKey $ Sophie.mkCredential stakingSK
              , SL.DCertPool  $ SL.RegPool $ poolParams unspentCoin
              , SL.DCertDeleg $ SL.Delegate $ SL.Delegation
                  { SL._delegator = Sophie.mkCredential stakingSK
                  , SL._delegatee = Sophie.mkKeyHash poolSK
                  }
              ]
          , SL._inputs   = Map.keysSet picked
          , SL._mdHash   = SL.SNothing
          , SL._outputs  =
              StrictSeq.singleton $ SL.TxOut sophieAddr unspentCoin
          , SL._ttl      = SlotNo maxBound
          , SL._txUpdate = SL.SNothing
          , SL._txfee    = fee
          , SL._wdrls    = SL.Wdrl Map.empty
          }

        bodyHash :: SL.SafeHash c SL.EraIndependentTxBody
        bodyHash = SL.hashAnnotated body

        -- Witness the use of bootstrap address's utxo.
        coleWit :: SL.BootstrapWitness c
        coleWit =
            SL.makeBootstrapWitness (SL.extractHash bodyHash) coleSK $
            Cole.addrAttributes coleAddr

        -- Witness the stake delegation.
        delegWit :: SL.WitVKey 'SL.Witness c
        delegWit =
            SL.makeWitnessVKey
              bodyHash
              (Sophie.mkKeyPair stakingSK)

        -- Witness the pool registration.
        poolWit :: SL.WitVKey 'SL.Witness c
        poolWit =
            SL.makeWitnessVKey
              bodyHash
              (Sophie.mkKeyPair poolSK)

    in
    if Map.null picked then Nothing else
    (Just . GenTxSophie. mkSophieTx) $
    SL.Tx
      { SL.body          = body
      , SL.auxiliaryData = SL.SNothing
      , SL.wits          = SL.WitnessSet
                             (Set.fromList [delegWit, poolWit])
                             mempty
                             (Set.singleton coleWit)
      }

    | otherwise           = Nothing

  where
    mbUTxO :: Maybe (SL.UTxO (SophieEra c))
    mbUTxO =
        fmap getUTxOSophie $
        ejectSophieTickedLedgerState $
        applyChainTick lcfg curSlot $
        lst

    MigrationInfo
      { coleMagic
      , coleSK
      , paymentSK
      , poolSK
      , stakingSK
      , vrfSK
      } = migrationInfo

    coleAddr :: Cole.Address
    coleAddr =
        Cole.makeVerKeyAddress coleMagic $ toVerification coleSK

    -- We use a base reference for the stake so that we can refer to it in the
    -- same tx that registers it.
    sophieAddr :: SL.Addr c
    sophieAddr =
        SL.Addr Sophie.networkId
          (Sophie.mkCredential paymentSK)
          (SL.StakeRefBase $ Sophie.mkCredential stakingSK)

    -- A simplistic individual pool
    poolParams :: SL.Coin -> SL.PoolParams c
    poolParams pledge = SL.PoolParams
        { SL._poolCost   = SL.Coin 1
        , SL._poolMD     = SL.SNothing
        , SL._poolMargin = minBound
        , SL._poolOwners = Set.singleton $ Sophie.mkKeyHash poolSK
        , SL._poolPledge = pledge
        , SL._poolId     = Sophie.mkKeyHash poolSK
        , SL._poolRAcnt  =
            SL.RewardAcnt Sophie.networkId $ Sophie.mkCredential poolSK
        , SL._poolRelays = StrictSeq.empty
        , SL._poolVrf    = Sophie.mkKeyHashVrf vrfSK
        }

-----

ejectSophieNS ::
     NS f (BccEras c)
  -> Maybe (f (SophieBlock (SophieEra c)))
ejectSophieNS = \case
    S (Z x) -> Just x
    _       -> Nothing

getUTxOSophie :: Ticked (LedgerState (SophieBlock era))
               -> SL.UTxO era
getUTxOSophie tls =
    SL._utxo $
    SL._utxoState $
    SL.esLState $
    SL.nesEs $
    tickedSophieLedgerState tls

ejectSophieTickedLedgerState ::
     Ticked (LedgerState (BccBlock c))
  -> Maybe (Ticked (LedgerState (SophieBlock (SophieEra c))))
ejectSophieTickedLedgerState ls =
    fmap (unComp . currentState) $
    ejectSophieNS $
    Tele.tip $
    getHardForkState $
    tickedHardForkLedgerStatePerEra $
    ls
