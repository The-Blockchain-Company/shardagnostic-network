{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Sophie mempool integration
module Shardagnostic.Consensus.Sophie.Ledger.Mempool (
    GenTx (..)
  , SL.ApplyTxError (..)
  , TxId (..)
  , Validated (..)
  , fixedBlockBodyOverhead
  , mkSophieTx
  , mkSophieValidatedTx
  , perTxOverhead
    -- * Exported for tests
  , AurumMeasure (..)
  ) where

import           Control.Monad.Except (Except)
import           Control.Monad.Identity (Identity (..))
import           Data.Foldable (toList)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           GHC.Records
import           NoThunks.Class (NoThunks (..))

import           Bcc.Binary (Annotator (..), FromCBOR (..),
                     FullByteString (..), ToCBOR (..))
import           Data.DerivingVia (InstantiatedAt (..))
import           Data.Measure (BoundedMeasure, Measure)

import           Shardagnostic.Network.Block (unwrapCBORinCBOR, wrapCBORinCBOR)

import           Bcc.Ledger.Aurum.Scripts (ExUnits (..))
import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.Ledger.SupportsMempool
import           Shardagnostic.Consensus.Mempool.TxLimits
import           Shardagnostic.Consensus.Util (ShowProxy (..))
import           Shardagnostic.Consensus.Util.Condense

import           Bcc.Ledger.Aurum.PParams
import           Bcc.Ledger.Aurum.Tx (ValidatedTx (..), totExUnits)
import qualified Bcc.Ledger.Core as Core (Tx)
import qualified Bcc.Ledger.Era as SL (Crypto, TxSeq, fromTxSeq)
import qualified Sophie.Spec.Ledger.API as SL
import qualified Sophie.Spec.Ledger.UTxO as SL (txid)

import           Shardagnostic.Consensus.Sophie.Eras
import           Shardagnostic.Consensus.Sophie.Ledger.Block
import           Shardagnostic.Consensus.Sophie.Ledger.Ledger
                     (SophieLedgerConfig (sophieLedgerGlobals),
                     Ticked (TickedSophieLedgerState, tickedSophieLedgerState),
                     getPParams)

data instance GenTx (SophieBlock era) = SophieTx !(SL.TxId (EraCrypto era)) !(Core.Tx era)
  deriving stock    (Generic)

deriving instance SophieBasedEra era => NoThunks (GenTx (SophieBlock era))

deriving instance SophieBasedEra era => Eq (GenTx (SophieBlock era))

instance Typeable era => ShowProxy (GenTx (SophieBlock era)) where

data instance Validated (GenTx (SophieBlock era)) =
    SophieValidatedTx
      !(SL.TxId (EraCrypto era))
      !(SL.Validated (Core.Tx era))
  deriving stock (Generic)

deriving instance SophieBasedEra era => NoThunks (Validated (GenTx (SophieBlock era)))

deriving instance SophieBasedEra era => Eq (Validated (GenTx (SophieBlock era)))

deriving instance SophieBasedEra era => Show (Validated (GenTx (SophieBlock era)))

instance Typeable era => ShowProxy (Validated (GenTx (SophieBlock era))) where

type instance ApplyTxErr (SophieBlock era) = SL.ApplyTxError era

-- orphaned instance
instance Typeable era => ShowProxy (SL.ApplyTxError era) where


-- |'txInBlockSize' is used to estimate how many transactions we can grab from
-- the Mempool to put into the block we are going to forge without exceeding
-- the maximum block body size according to the ledger. If we exceed that
-- limit, we will have forged a block that is invalid according to the ledger.
-- We ourselves won't even adopt it, causing us to lose our slot, something we
-- must try to avoid.
--
-- For this reason it is better to overestimate the size of a transaction than
-- to underestimate. The only downside is that we maybe could have put one (or
-- more?) transactions extra in that block.
--
-- As the sum of the serialised transaction sizes is not equal to the size of
-- the serialised block body ('SL.TxSeq') consisting of those transactions
-- (see bcc-node#1545 for an example), we account for some extra overhead
-- per transaction as a safety margin.
--
-- Also see 'perTxOverhead'.
fixedBlockBodyOverhead :: Num a => a
fixedBlockBodyOverhead = 1024

-- | See 'fixedBlockBodyOverhead'.
perTxOverhead :: Num a => a
perTxOverhead = 4

instance SophieBasedEra era
      => LedgerSupportsMempool (SophieBlock era) where
  txInvariant = const True

  applyTx = applySophieTx

  reapplyTx = reapplySophieTx

  txsMaxBytes TickedSophieLedgerState { tickedSophieLedgerState = sophieState } =
      fromIntegral maxBlockBodySize - fixedBlockBodyOverhead
    where
      maxBlockBodySize = getField @"_maxBBSize" $ getPParams sophieState

  txInBlockSize (SophieTx _ tx) = txSize + perTxOverhead
    where
      txSize = fromIntegral $ getField @"txsize" tx

  txForgetValidated (SophieValidatedTx txid vtx) = SophieTx txid (SL.extractTx vtx)

mkSophieTx :: forall era. SophieBasedEra era => Core.Tx era -> GenTx (SophieBlock era)
mkSophieTx tx = SophieTx (SL.txid @era (getField @"body" tx)) tx

mkSophieValidatedTx :: forall era.
     SophieBasedEra era
  => SL.Validated (Core.Tx era)
  -> Validated (GenTx (SophieBlock era))
mkSophieValidatedTx vtx = SophieValidatedTx txid vtx
  where
    txid = SL.txid @era (getField @"body" (SL.extractTx vtx))

newtype instance TxId (GenTx (SophieBlock era)) = SophieTxId (SL.TxId (EraCrypto era))
  deriving newtype (Eq, Ord, NoThunks)

deriving newtype instance (SL.OptimumCrypto (EraCrypto era), Typeable era)
                       => ToCBOR (TxId (GenTx (SophieBlock era)))
deriving newtype instance (SL.OptimumCrypto (EraCrypto era), Typeable era)
                       => FromCBOR (TxId (GenTx (SophieBlock era)))

instance Typeable era => ShowProxy (TxId (GenTx (SophieBlock era))) where

instance SophieBasedEra era => HasTxId (GenTx (SophieBlock era)) where
  txId (SophieTx i _) = SophieTxId i

instance SophieBasedEra era => HasTxs (SophieBlock era) where
  extractTxs =
        map mkSophieTx
      . txSeqToList
      . SL.bbody
      . sophieBlockRaw
    where
      txSeqToList :: SL.TxSeq era -> [Core.Tx era]
      txSeqToList = toList . SL.fromTxSeq @era

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance SophieBasedEra era => ToCBOR (GenTx (SophieBlock era)) where
  -- No need to encode the 'TxId', it's just a hash of the 'SL.TxBody' inside
  -- 'SL.Tx', so it can be recomputed.
  toCBOR (SophieTx _txid tx) = wrapCBORinCBOR toCBOR tx

instance SophieBasedEra era => FromCBOR (GenTx (SophieBlock era)) where
  fromCBOR = fmap mkSophieTx $ unwrapCBORinCBOR
    $ (. Full) . runAnnotator <$> fromCBOR

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance SophieBasedEra era => Condense (GenTx (SophieBlock era)) where
  condense (SophieTx _ tx ) = show tx

instance Condense (GenTxId (SophieBlock era)) where
  condense (SophieTxId i) = "txid: " <> show i

instance SophieBasedEra era => Show (GenTx (SophieBlock era)) where
  show = condense

instance Show (GenTxId (SophieBlock era)) where
  show = condense

{-------------------------------------------------------------------------------
  Applying transactions
-------------------------------------------------------------------------------}

applySophieTx :: forall era.
     SophieBasedEra era
  => LedgerConfig (SophieBlock era)
  -> WhetherToIntervene
  -> SlotNo
  -> GenTx (SophieBlock era)
  -> TickedLedgerState (SophieBlock era)
  -> Except (ApplyTxErr (SophieBlock era))
       ( TickedLedgerState (SophieBlock era)
       , Validated (GenTx (SophieBlock era))
       )
applySophieTx cfg wti slot (SophieTx _ tx) st = do
    (mempoolState', vtx) <-
       applySophieBasedTx
         (sophieLedgerGlobals cfg)
         (SL.mkMempoolEnv   innerSt slot)
         (SL.mkMempoolState innerSt)
         wti
         tx

    let st' = set theLedgerLens mempoolState' st

    pure (st', mkSophieValidatedTx vtx)
  where
    innerSt = tickedSophieLedgerState st

reapplySophieTx ::
     SophieBasedEra era
  => LedgerConfig (SophieBlock era)
  -> SlotNo
  -> Validated (GenTx (SophieBlock era))
  -> TickedLedgerState (SophieBlock era)
  -> Except (ApplyTxErr (SophieBlock era)) (TickedLedgerState (SophieBlock era))
reapplySophieTx cfg slot vgtx st = do
    mempoolState' <-
        SL.reapplyTx
          (sophieLedgerGlobals cfg)
          (SL.mkMempoolEnv   innerSt slot)
          (SL.mkMempoolState innerSt)
          vtx

    pure $ set theLedgerLens mempoolState' st
  where
    SophieValidatedTx _txid vtx = vgtx

    innerSt = tickedSophieLedgerState st

-- | The lens combinator
set ::
     (forall f. Applicative f => (a -> f b) -> s -> f t)
  -> b -> s -> t
set lens inner outer =
    runIdentity $ lens (\_ -> Identity inner) outer

theLedgerLens ::
     -- TODO SL.overNewEpochState should not require 'Applicative'
     Applicative f
  => (      (SL.UTxOState era, SL.DPState (SL.Crypto era))
       -> f (SL.UTxOState era, SL.DPState (SL.Crypto era))
     )
  ->    TickedLedgerState (SophieBlock era)
  -> f (TickedLedgerState (SophieBlock era))
theLedgerLens f x =
        (\y -> x{tickedSophieLedgerState = y})
    <$> SL.overNewEpochState f (tickedSophieLedgerState x)

{-------------------------------------------------------------------------------
  Tx Limits
-------------------------------------------------------------------------------}

instance (SL.OptimumCrypto c) => TxLimits (SophieBlock (SophieEra c)) where
  type TxMeasure (SophieBlock (SophieEra c)) = ByteSize
  txMeasure        = ByteSize . txInBlockSize . txForgetValidated
  txsBlockCapacity = ByteSize . txsMaxBytes

instance (SL.OptimumCrypto c) => TxLimits (SophieBlock (EvieEra c)) where
  type TxMeasure (SophieBlock (EvieEra c)) = ByteSize
  txMeasure        = ByteSize . txInBlockSize . txForgetValidated
  txsBlockCapacity = ByteSize . txsMaxBytes

instance (SL.OptimumCrypto c) => TxLimits (SophieBlock (JenEra c)) where
  type TxMeasure (SophieBlock (JenEra c)) = ByteSize
  txMeasure        = ByteSize . txInBlockSize . txForgetValidated
  txsBlockCapacity = ByteSize . txsMaxBytes

instance ( SL.OptimumCrypto c
         ) => TxLimits (SophieBlock (AurumEra c)) where

  type TxMeasure (SophieBlock (AurumEra c)) = AurumMeasure

  txMeasure (SophieValidatedTx _txid vtx) =
    AurumMeasure {
        byteSize = ByteSize $ txInBlockSize (mkSophieTx @(AurumEra c) (SL.extractTx vtx))
      , exUnits  = totExUnits (SL.extractTx vtx)
      }

  txsBlockCapacity ledgerState =
      AurumMeasure {
          byteSize = ByteSize $ txsMaxBytes ledgerState
        , exUnits  = getField @"_maxBlockExUnits" pparams
        }
    where
      pparams = getPParams $ tickedSophieLedgerState ledgerState

data AurumMeasure = AurumMeasure {
    byteSize :: !ByteSize
  , exUnits  :: !ExUnits
  } deriving stock (Eq, Generic, Show)
    deriving (BoundedMeasure, Measure)
         via (InstantiatedAt Generic AurumMeasure)
