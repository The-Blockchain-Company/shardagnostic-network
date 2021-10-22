{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE ViewPatterns             #-}
module Shardagnostic.Consensus.Bcc.Block (
    -- * Eras
    BccEras
  , module Shardagnostic.Consensus.Sophie.Eras
  , SophieBasedEras
    -- * Block
  , BccBlock
    -- Note: by exporting the pattern synonyms as part of the matching data
    -- type (instead of as separate patterns), we get better exhaustiveness
    -- checks from GHC. But GHC expects a data type, not a type family, that's
    -- why we sometimes mention the data type of the instance in these exports
    -- instead of the abstract type family.
  , HardForkBlock (BlockEvie, BlockAurum, BlockCole, BlockJen, BlockSophie)
    -- * Headers
  , BccHeader
  , Header (HeaderEvie, HeaderAurum, HeaderCole, HeaderJen, HeaderSophie)
    -- * Generalised transactions
  , BccApplyTxErr
  , BccGenTx
  , BccGenTxId
  , GenTx (GenTxEvie, GenTxAurum, GenTxCole, GenTxJen, GenTxSophie)
  , HardForkApplyTxErr (ApplyTxErrEvie, ApplyTxErrAurum, ApplyTxErrCole, ApplyTxErrJen, ApplyTxErrSophie, ApplyTxErrWrongEra)
  , TxId (GenTxIdEvie, GenTxIdAurum, GenTxIdCole, GenTxIdJen, GenTxIdSophie)
    -- * LedgerError
  , BccLedgerError
  , HardForkLedgerError (LedgerErrorEvie, LedgerErrorAurum, LedgerErrorCole, LedgerErrorJen, LedgerErrorSophie, LedgerErrorWrongEra)
    -- * OtherEnvelopeError
  , BccOtherHeaderEnvelopeError
  , HardForkEnvelopeErr (OtherHeaderEnvelopeErrorEvie, OtherHeaderEnvelopeErrorAurum, OtherHeaderEnvelopeErrorCole, OtherHeaderEnvelopeErrorJen, OtherHeaderEnvelopeErrorSophie, OtherHeaderEnvelopeErrorWrongEra)
    -- * TipInfo
  , BccTipInfo
  , OneEraTipInfo (TipInfoEvie, TipInfoAurum, TipInfoCole, TipInfoJen, TipInfoSophie)
    -- * Query
  , BlockQuery (QueryAnytimeEvie, QueryAnytimeAurum, QueryAnytimeCole, QueryAnytimeJen, QueryAnytimeSophie, QueryHardFork, QueryIfCurrentEvie, QueryIfCurrentAurum, QueryIfCurrentCole, QueryIfCurrentJen, QueryIfCurrentSophie)
  , BccQuery
  , BccQueryResult
  , Either (QueryResultSuccess, QueryResultEraMismatch)
    -- * CodecConfig
  , BccCodecConfig
  , CodecConfig (BccCodecConfig)
    -- * BlockConfig
  , BlockConfig (BccBlockConfig)
  , BccBlockConfig
    -- * StorageConfig
  , BccStorageConfig
  , StorageConfig (BccStorageConfig)
    -- * ConsensusConfig
  , BccConsensusConfig
  , ConsensusConfig (BccConsensusConfig)
    -- * LedgerConfig
  , BccLedgerConfig
  , HardForkLedgerConfig (BccLedgerConfig)
    -- * LedgerState
  , BccLedgerState
  , LedgerState (LedgerStateEvie, LedgerStateAurum, LedgerStateCole, LedgerStateJen, LedgerStateSophie)
    -- * ChainDepState
  , BccChainDepState
  , HardForkState (ChainDepStateEvie, ChainDepStateAurum, ChainDepStateCole, ChainDepStateJen, ChainDepStateSophie)
    -- * EraMismatch
  , EraMismatch (..)
  ) where

import           Data.SOP.Strict

import           Shardagnostic.Consensus.Block (BlockProtocol)
import           Shardagnostic.Consensus.HeaderValidation (OtherHeaderEnvelopeError,
                     TipInfo)
import           Shardagnostic.Consensus.Ledger.Abstract (LedgerError)
import           Shardagnostic.Consensus.Ledger.SupportsMempool (ApplyTxErr,
                     GenTxId)
import           Shardagnostic.Consensus.Protocol.Abstract (ChainDepState)
import           Shardagnostic.Consensus.TypeFamilyWrappers

import           Shardagnostic.Consensus.HardFork.Combinator
import           Shardagnostic.Consensus.HardFork.Combinator.AcrossEras
import qualified Shardagnostic.Consensus.HardFork.Combinator.State as State

import           Shardagnostic.Consensus.Cole.Ledger.Block (ColeBlock)

import           Shardagnostic.Consensus.Sophie.Eras
import           Shardagnostic.Consensus.Sophie.Ledger (SophieBlock)

{-------------------------------------------------------------------------------
  The eras of the Bcc bock chain
-------------------------------------------------------------------------------}

-- | The eras in the Bcc blockchain.
--
-- We parameterise over the crypto used in the post-Cole eras: @c@.
--
-- TODO: parameterise ColeBlock over crypto too
type BccEras c =
  '[ ColeBlock
   , SophieBlock (SophieEra c)
   , SophieBlock (EvieEra c)
   , SophieBlock (JenEra c)
   , SophieBlock (AurumEra c)
   ]

-- | The Sophie-based eras in the Bcc chain
type SophieBasedEras c = '[SophieEra c, EvieEra c, JenEra c, AurumEra c]

{-------------------------------------------------------------------------------
  INTERNAL A tag function for each era
-------------------------------------------------------------------------------}

-- Here we use layout and adjacency to make it obvious that we haven't
-- miscounted.

pattern TagCole   :: f ColeBlock                    -> NS f (BccEras c)
pattern TagSophie :: f (SophieBlock (SophieEra c)) -> NS f (BccEras c)
pattern TagEvie :: f (SophieBlock (EvieEra c)) -> NS f (BccEras c)
pattern TagJen    :: f (SophieBlock (JenEra    c)) -> NS f (BccEras c)
pattern TagAurum  :: f (SophieBlock (AurumEra  c)) -> NS f (BccEras c)

pattern TagCole   x =             Z x
pattern TagSophie x =          S (Z x)
pattern TagEvie x =       S (S (Z x))
pattern TagJen    x =    S (S (S (Z x)))
pattern TagAurum  x = S (S (S (S (Z x))))

{-------------------------------------------------------------------------------
  INTERNAL A telescope function for each era

-------------------------------------------------------------------------------}

pattern TeleCole   ::
     f ColeBlock
  -> Telescope g f (BccEras c)

pattern TeleSophie ::
     g ColeBlock
  -> f (SophieBlock (SophieEra c))
  -> Telescope g f (BccEras c)

pattern TeleEvie ::
     g ColeBlock
  -> g (SophieBlock (SophieEra c))
  -> f (SophieBlock (EvieEra c))
  -> Telescope g f (BccEras c)

pattern TeleJen    ::
     g ColeBlock
  -> g (SophieBlock (SophieEra c))
  -> g (SophieBlock (EvieEra c))
  -> f (SophieBlock (JenEra    c))
  -> Telescope g f (BccEras c)

pattern TeleAurum  ::
     g ColeBlock
  -> g (SophieBlock (SophieEra c))
  -> g (SophieBlock (EvieEra c))
  -> g (SophieBlock (JenEra    c))
  -> f (SophieBlock (AurumEra  c))
  -> Telescope g f (BccEras c)

-- Here we use layout and adjacency to make it obvious that we haven't
-- miscounted.

pattern TeleCole                              x =                                            TZ x
pattern TeleSophie cole                      x = TS cole                                  (TZ x)
pattern TeleEvie cole sophie              x = TS cole (TS sophie                      (TZ x))
pattern TeleJen    cole sophie evie      x = TS cole (TS sophie (TS evie          (TZ x)))
pattern TeleAurum  cole sophie evie jen x = TS cole (TS sophie (TS evie (TS jen (TZ x))))

{-------------------------------------------------------------------------------
  The block type of the Bcc block chain
-------------------------------------------------------------------------------}

-- | /The/ Bcc block.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'BlockCole' and 'BlockSophie'.
--
-- > f :: BccBlock c -> _
-- > f (BlockCole   b) = _
-- > f (BlockSophie s) = _
-- > f (BlockEvie a) = _
-- > f (BlockJen    m) = _
-- > f (BlockAurum  m) = _
--
type BccBlock c = HardForkBlock (BccEras c)

pattern BlockCole :: ColeBlock -> BccBlock c
pattern BlockCole b = HardForkBlock (OneEraBlock (TagCole (I b)))

pattern BlockSophie :: SophieBlock (SophieEra c) -> BccBlock c
pattern BlockSophie b = HardForkBlock (OneEraBlock (TagSophie (I b)))

pattern BlockEvie :: SophieBlock (EvieEra c) -> BccBlock c
pattern BlockEvie b = HardForkBlock (OneEraBlock (TagEvie (I b)))

pattern BlockJen :: SophieBlock (JenEra c) -> BccBlock c
pattern BlockJen b = HardForkBlock (OneEraBlock (TagJen (I b)))

pattern BlockAurum :: SophieBlock (AurumEra c) -> BccBlock c
pattern BlockAurum b = HardForkBlock (OneEraBlock (TagAurum (I b)))

{-# COMPLETE BlockCole, BlockSophie, BlockEvie, BlockJen, BlockAurum #-}

{-------------------------------------------------------------------------------
  Headers
-------------------------------------------------------------------------------}

-- | The Bcc header.
type BccHeader c = Header (BccBlock c)

pattern HeaderCole :: Header ColeBlock -> BccHeader c
pattern HeaderCole h = HardForkHeader (OneEraHeader (TagCole h))

pattern HeaderSophie ::
     Header (SophieBlock (SophieEra c))
  -> BccHeader c
pattern HeaderSophie h = HardForkHeader (OneEraHeader (TagSophie h))

pattern HeaderEvie ::
     Header (SophieBlock (EvieEra c))
  -> BccHeader c
pattern HeaderEvie h = HardForkHeader (OneEraHeader (TagEvie h))

pattern HeaderJen ::
     Header (SophieBlock (JenEra c))
  -> BccHeader c
pattern HeaderJen h = HardForkHeader (OneEraHeader (TagJen h))

pattern HeaderAurum ::
     Header (SophieBlock (AurumEra c))
  -> BccHeader c
pattern HeaderAurum h = HardForkHeader (OneEraHeader (TagAurum h))

{-# COMPLETE HeaderCole
           , HeaderSophie
           , HeaderEvie
           , HeaderJen
           , HeaderAurum #-}

{-------------------------------------------------------------------------------
  Generalised transactions
-------------------------------------------------------------------------------}

-- | The Bcc transaction.
type BccGenTx c = GenTx (BccBlock c)

pattern GenTxCole :: GenTx ColeBlock -> BccGenTx c
pattern GenTxCole tx = HardForkGenTx (OneEraGenTx (TagCole tx))

pattern GenTxSophie :: GenTx (SophieBlock (SophieEra c)) -> BccGenTx c
pattern GenTxSophie tx = HardForkGenTx (OneEraGenTx (TagSophie tx))

pattern GenTxEvie :: GenTx (SophieBlock (EvieEra c)) -> BccGenTx c
pattern GenTxEvie tx = HardForkGenTx (OneEraGenTx (TagEvie tx))

pattern GenTxJen :: GenTx (SophieBlock (JenEra c)) -> BccGenTx c
pattern GenTxJen tx = HardForkGenTx (OneEraGenTx (TagJen tx))

pattern GenTxAurum :: GenTx (SophieBlock (AurumEra c)) -> BccGenTx c
pattern GenTxAurum tx = HardForkGenTx (OneEraGenTx (TagAurum tx))

{-# COMPLETE GenTxCole, GenTxSophie, GenTxEvie, GenTxJen, GenTxAurum #-}

-- | The ID of a Bcc transaction.
type BccGenTxId c = GenTxId (BccBlock c)

pattern GenTxIdCole :: GenTxId ColeBlock -> BccGenTxId c
pattern GenTxIdCole txid =
    HardForkGenTxId (OneEraGenTxId (TagCole (WrapGenTxId txid)))

pattern GenTxIdSophie ::
     GenTxId (SophieBlock (SophieEra c))
  -> BccGenTxId c
pattern GenTxIdSophie txid =
    HardForkGenTxId (OneEraGenTxId (TagSophie (WrapGenTxId txid)))

pattern GenTxIdEvie ::
     GenTxId (SophieBlock (EvieEra c))
  -> BccGenTxId c
pattern GenTxIdEvie txid =
    HardForkGenTxId (OneEraGenTxId (TagEvie (WrapGenTxId txid)))

pattern GenTxIdJen ::
     GenTxId (SophieBlock (JenEra c))
  -> BccGenTxId c
pattern GenTxIdJen txid =
    HardForkGenTxId (OneEraGenTxId (TagJen (WrapGenTxId txid)))

pattern GenTxIdAurum ::
     GenTxId (SophieBlock (AurumEra c))
  -> BccGenTxId c
pattern GenTxIdAurum txid =
    HardForkGenTxId (OneEraGenTxId (TagAurum (WrapGenTxId txid)))

{-# COMPLETE GenTxIdCole
           , GenTxIdSophie
           , GenTxIdEvie
           , GenTxIdJen
           , GenTxIdAurum #-}

-- | An error resulting from applying a 'BccGenTx' to the ledger.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'ApplyTxColeErr', 'ApplyTxErrSophie', and
-- 'ApplyTxErrWrongEra'.
--
-- > toText :: BccApplyTxErr c -> Text
-- > toText (ApplyTxErrCole b) = coleApplyTxErrToText b
-- > toText (ApplyTxErrSophie s) = sophieApplyTxErrToText s
-- > toText (ApplyTxErrEvie a) = evieApplyTxErrToText a
-- > toText (ApplyTxErrJen m) = jenApplyTxErrToText m
-- > toText (ApplyTxErrWrongEra eraMismatch) =
-- >   "Transaction from the " <> otherEraName eraMismatch <>
-- >   " era applied to a ledger from the " <>
-- >   ledgerEraName eraMismatch <> " era"
--
type BccApplyTxErr c = HardForkApplyTxErr (BccEras c)

pattern ApplyTxErrCole :: ApplyTxErr ColeBlock -> BccApplyTxErr c
pattern ApplyTxErrCole err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagCole (WrapApplyTxErr err)))

pattern ApplyTxErrSophie ::
     ApplyTxErr (SophieBlock (SophieEra c))
  -> BccApplyTxErr c
pattern ApplyTxErrSophie err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagSophie (WrapApplyTxErr err)))

pattern ApplyTxErrEvie ::
     ApplyTxErr (SophieBlock (EvieEra c))
  -> BccApplyTxErr c
pattern ApplyTxErrEvie err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagEvie (WrapApplyTxErr err)))

pattern ApplyTxErrJen ::
     ApplyTxErr (SophieBlock (JenEra c))
  -> BccApplyTxErr c
pattern ApplyTxErrJen err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagJen (WrapApplyTxErr err)))

pattern ApplyTxErrAurum ::
     ApplyTxErr (SophieBlock (AurumEra c))
  -> BccApplyTxErr c
pattern ApplyTxErrAurum err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagAurum (WrapApplyTxErr err)))

pattern ApplyTxErrWrongEra :: EraMismatch -> BccApplyTxErr c
pattern ApplyTxErrWrongEra eraMismatch <-
    HardForkApplyTxErrWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE ApplyTxErrCole
           , ApplyTxErrSophie
           , ApplyTxErrEvie
           , ApplyTxErrJen
           , ApplyTxErrAurum
           , ApplyTxErrWrongEra #-}

{-------------------------------------------------------------------------------
  LedgerError
-------------------------------------------------------------------------------}

-- | An error resulting from applying a 'BccBlock' to the ledger.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'LedgerErrorCole', 'LedgerErrorSophie', and
-- 'LedgerErrorWrongEra'.
--
-- > toText :: BccLedgerError c -> Text
-- > toText (LedgerErrorCole b) = coleLedgerErrorToText b
-- > toText (LedgerErrorSophie s) = sophieLedgerErrorToText s
-- > toText (LedgerErrorEvie a) = evieLedgerErrorToText a
-- > toText (LedgerErrorJen m) = jenLedgerErrorToText m
-- > toText (LedgerErrorWrongEra eraMismatch) =
-- >   "Block from the " <> otherEraName eraMismatch <>
-- >   " era applied to a ledger from the " <>
-- >   ledgerEraName eraMismatch <> " era"
--
type BccLedgerError c = HardForkLedgerError (BccEras c)

pattern LedgerErrorCole :: LedgerError ColeBlock -> BccLedgerError c
pattern LedgerErrorCole err =
    HardForkLedgerErrorFromEra (OneEraLedgerError (TagCole (WrapLedgerErr err)))

pattern LedgerErrorSophie ::
     LedgerError (SophieBlock (SophieEra c))
  -> BccLedgerError c
pattern LedgerErrorSophie err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (TagSophie (WrapLedgerErr err)))

pattern LedgerErrorEvie ::
     LedgerError (SophieBlock (EvieEra c))
  -> BccLedgerError c
pattern LedgerErrorEvie err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (TagEvie (WrapLedgerErr err)))

pattern LedgerErrorJen ::
     LedgerError (SophieBlock (JenEra c))
  -> BccLedgerError c
pattern LedgerErrorJen err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (TagJen (WrapLedgerErr err)))

pattern LedgerErrorAurum ::
     LedgerError (SophieBlock (AurumEra c))
  -> BccLedgerError c
pattern LedgerErrorAurum err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (TagAurum (WrapLedgerErr err)))

pattern LedgerErrorWrongEra :: EraMismatch -> BccLedgerError c
pattern LedgerErrorWrongEra eraMismatch <-
    HardForkLedgerErrorWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE LedgerErrorCole
           , LedgerErrorSophie
           , LedgerErrorEvie
           , LedgerErrorJen
           , LedgerErrorAurum
           , LedgerErrorWrongEra #-}

{-------------------------------------------------------------------------------
  OtherEnvelopeError
-------------------------------------------------------------------------------}

-- | An error resulting from validating a 'BccHeader'.
type BccOtherHeaderEnvelopeError c = HardForkEnvelopeErr (BccEras c)

pattern OtherHeaderEnvelopeErrorCole
  :: OtherHeaderEnvelopeError ColeBlock
  -> BccOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorCole err =
    HardForkEnvelopeErrFromEra
      (OneEraEnvelopeErr (TagCole (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorSophie
  :: OtherHeaderEnvelopeError (SophieBlock (SophieEra c))
  -> BccOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorSophie err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (TagSophie (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorEvie
  :: OtherHeaderEnvelopeError (SophieBlock (EvieEra c))
  -> BccOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorEvie err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (TagEvie (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorJen
  :: OtherHeaderEnvelopeError (SophieBlock (JenEra c))
  -> BccOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorJen err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (TagJen (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorAurum
  :: OtherHeaderEnvelopeError (SophieBlock (AurumEra c))
  -> BccOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorAurum err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (TagAurum (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorWrongEra
  :: EraMismatch
  -> BccOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorWrongEra eraMismatch <-
    HardForkEnvelopeErrWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE OtherHeaderEnvelopeErrorCole
           , OtherHeaderEnvelopeErrorSophie
           , OtherHeaderEnvelopeErrorEvie
           , OtherHeaderEnvelopeErrorJen
           , OtherHeaderEnvelopeErrorAurum
           , OtherHeaderEnvelopeErrorWrongEra #-}

{-------------------------------------------------------------------------------
  TipInfo
-------------------------------------------------------------------------------}

-- | The 'TipInfo' of the Bcc chain.
type BccTipInfo c = OneEraTipInfo (BccEras c)

pattern TipInfoCole :: TipInfo ColeBlock -> BccTipInfo c
pattern TipInfoCole ti = OneEraTipInfo (TagCole (WrapTipInfo ti))

pattern TipInfoSophie ::
     TipInfo (SophieBlock (SophieEra c))
  -> BccTipInfo c
pattern TipInfoSophie ti = OneEraTipInfo (TagSophie (WrapTipInfo ti))

pattern TipInfoEvie ::
     TipInfo (SophieBlock (EvieEra c))
  -> BccTipInfo c
pattern TipInfoEvie ti = OneEraTipInfo (TagEvie (WrapTipInfo ti))

pattern TipInfoJen ::
     TipInfo (SophieBlock (JenEra c))
  -> BccTipInfo c
pattern TipInfoJen ti = OneEraTipInfo (TagJen (WrapTipInfo ti))

pattern TipInfoAurum ::
     TipInfo (SophieBlock (AurumEra c))
  -> BccTipInfo c
pattern TipInfoAurum ti = OneEraTipInfo (TagAurum (WrapTipInfo ti))

{-# COMPLETE TipInfoCole
           , TipInfoSophie
           , TipInfoEvie
           , TipInfoJen
           , TipInfoAurum #-}

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | The 'Query' of Bcc chain.
type BccQuery c = BlockQuery (BccBlock c)

-- | Cole-specific query that can only be answered when the ledger is in the
-- Cole era.
pattern QueryIfCurrentCole
  :: ()
  => BccQueryResult c result ~ a
  => BlockQuery ColeBlock result
  -> BccQuery c a

-- | Sophie-specific query that can only be answered when the ledger is in the
-- Sophie era.
pattern QueryIfCurrentSophie
  :: ()
  => BccQueryResult c result ~ a
  => BlockQuery (SophieBlock (SophieEra c)) result
  -> BccQuery c a

-- | Evie-specific query that can only be answered when the ledger is in the
-- Evie era.
pattern QueryIfCurrentEvie
  :: ()
  => BccQueryResult c result ~ a
  => BlockQuery (SophieBlock (EvieEra c)) result
  -> BccQuery c a

-- | Jen-specific query that can only be answered when the ledger is in the
-- Jen era.
pattern QueryIfCurrentJen
  :: ()
  => BccQueryResult c result ~ a
  => BlockQuery (SophieBlock (JenEra c)) result
  -> BccQuery c a

-- | Aurum-specific query that can only be answered when the ledger is in the
-- Aurum era.
pattern QueryIfCurrentAurum
  :: ()
  => BccQueryResult c result ~ a
  => BlockQuery (SophieBlock (AurumEra c)) result
  -> BccQuery c a

-- Here we use layout and adjacency to make it obvious that we haven't
-- miscounted.

pattern QueryIfCurrentCole   q = QueryIfCurrent                 (QZ q)
pattern QueryIfCurrentSophie q = QueryIfCurrent             (QS (QZ q))
pattern QueryIfCurrentEvie q = QueryIfCurrent         (QS (QS (QZ q)))
pattern QueryIfCurrentJen    q = QueryIfCurrent     (QS (QS (QS (QZ q))))
pattern QueryIfCurrentAurum  q = QueryIfCurrent (QS (QS (QS (QS (QZ q)))))

-- | Query about the Cole era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Cole era (whether the tip of
-- the ledger is in the Cole, Sophie, ... era), use:
--
-- > QueryAnytimeCole EraStart
--
pattern QueryAnytimeCole
  :: QueryAnytime result
  -> BccQuery c result
pattern QueryAnytimeCole q = QueryAnytime q (EraIndex (TagCole (K ())))

-- | Query about the Sophie era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Sophie era (whether the tip of the
-- ledger is in the Cole, Sophie, ... era), use:
--
-- > QueryAnytimeSophie EraStart
--
pattern QueryAnytimeSophie
  :: QueryAnytime result
  -> BccQuery c result
pattern QueryAnytimeSophie q = QueryAnytime q (EraIndex (TagSophie (K ())))

-- | Query about the Evie era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Evie era (whether the tip of the
-- ledger is in the Cole, Sophie, ... era), use:
--
-- > QueryAnytimeEvie EraStart
--
pattern QueryAnytimeEvie
  :: QueryAnytime result
  -> BccQuery c result
pattern QueryAnytimeEvie q = QueryAnytime q (EraIndex (TagEvie (K ())))

-- | Query about the Jen era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Jen era (whether the tip of the
-- ledger is in the Cole, Sophie, ... era), use:
--
-- > QueryAnytimeJen EraStart
--
pattern QueryAnytimeJen
  :: QueryAnytime result
  -> BccQuery c result
pattern QueryAnytimeJen q = QueryAnytime q (EraIndex (TagJen (K ())))

-- | Query about the Aurum era that can be answered anytime, i.e., independent
-- from where the tip of the ledger is.
--
-- For example, to ask for the start of the Aurum era (whether the tip of the
-- ledger is in the Cole, Sophie, ... era), use:
--
-- > QueryAnytimeAurum EraStart
--
pattern QueryAnytimeAurum
  :: QueryAnytime result
  -> BccQuery c result
pattern QueryAnytimeAurum q = QueryAnytime q (EraIndex (TagAurum (K ())))

{-# COMPLETE QueryIfCurrentCole
           , QueryIfCurrentSophie
           , QueryIfCurrentEvie
           , QueryIfCurrentJen
           , QueryIfCurrentAurum
           , QueryAnytimeCole
           , QueryAnytimeSophie
           , QueryAnytimeEvie
           , QueryAnytimeJen
           , QueryAnytimeAurum
           , QueryHardFork #-}

-- | The result of a 'BccQuery'
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'QueryResultSuccess' and 'QueryResultEraMismatch'.
type BccQueryResult c = HardForkQueryResult (BccEras c)

pattern QueryResultSuccess :: result -> BccQueryResult c result
pattern QueryResultSuccess result = Right result

-- | A query from a different era than the ledger's era was sent.
pattern QueryResultEraMismatch :: EraMismatch -> BccQueryResult c result
pattern QueryResultEraMismatch eraMismatch <- Left (mkEraMismatch -> eraMismatch)

{-# COMPLETE QueryResultSuccess, QueryResultEraMismatch #-}

{-------------------------------------------------------------------------------
  CodecConfig
-------------------------------------------------------------------------------}

-- | The 'CodecConfig' for 'BccBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Cole, Sophie, ... 'CodecConfig's.
type BccCodecConfig c = CodecConfig (BccBlock c)

pattern BccCodecConfig
  :: CodecConfig ColeBlock
  -> CodecConfig (SophieBlock (SophieEra c))
  -> CodecConfig (SophieBlock (EvieEra c))
  -> CodecConfig (SophieBlock (JenEra c))
  -> CodecConfig (SophieBlock (AurumEra c))
  -> BccCodecConfig c
pattern BccCodecConfig cfgCole cfgSophie cfgEvie cfgJen cfgAurum =
    HardForkCodecConfig {
        hardForkCodecConfigPerEra = PerEraCodecConfig
          (  cfgCole
          :* cfgSophie
          :* cfgEvie
          :* cfgJen
          :* cfgAurum
          :* Nil
          )
      }

{-# COMPLETE BccCodecConfig #-}

{-------------------------------------------------------------------------------
  BlockConfig
-------------------------------------------------------------------------------}

-- | The 'BlockConfig' for 'BccBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Cole, Sophie, ... 'BlockConfig's.
type BccBlockConfig c = BlockConfig (BccBlock c)

pattern BccBlockConfig
  :: BlockConfig ColeBlock
  -> BlockConfig (SophieBlock (SophieEra c))
  -> BlockConfig (SophieBlock (EvieEra c))
  -> BlockConfig (SophieBlock (JenEra c))
  -> BlockConfig (SophieBlock (AurumEra c))
  -> BccBlockConfig c
pattern BccBlockConfig cfgCole cfgSophie cfgEvie cfgJen cfgAurum =
    HardForkBlockConfig {
        hardForkBlockConfigPerEra = PerEraBlockConfig
          (  cfgCole
          :* cfgSophie
          :* cfgEvie
          :* cfgJen
          :* cfgAurum
          :* Nil
          )
      }

{-# COMPLETE BccBlockConfig #-}

{-------------------------------------------------------------------------------
  StorageConfig
-------------------------------------------------------------------------------}

-- | The 'StorageConfig' for 'BccBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Cole, Sophie, ... 'StorageConfig's.
type BccStorageConfig c = StorageConfig (BccBlock c)

pattern BccStorageConfig
  :: StorageConfig ColeBlock
  -> StorageConfig (SophieBlock (SophieEra c))
  -> StorageConfig (SophieBlock (EvieEra c))
  -> StorageConfig (SophieBlock (JenEra c))
  -> StorageConfig (SophieBlock (AurumEra c))
  -> BccStorageConfig c
pattern BccStorageConfig cfgCole cfgSophie cfgEvie cfgJen cfgAurum =
    HardForkStorageConfig {
        hardForkStorageConfigPerEra = PerEraStorageConfig
          (  cfgCole
          :* cfgSophie
          :* cfgEvie
          :* cfgJen
          :* cfgAurum
          :* Nil
          )
      }

{-# COMPLETE BccStorageConfig #-}

{-------------------------------------------------------------------------------
  ConsensusConfig
-------------------------------------------------------------------------------}

-- | The 'ConsensusConfig' for 'BccBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of the
-- Cole, Sophie, ... 'PartialConsensusConfig's.
--
-- NOTE: not 'ConsensusConfig', but 'PartialConsensusConfig'.
type BccConsensusConfig c =
  ConsensusConfig (HardForkProtocol (BccEras c))

pattern BccConsensusConfig
  :: PartialConsensusConfig (BlockProtocol ColeBlock)
  -> PartialConsensusConfig (BlockProtocol (SophieBlock (SophieEra c)))
  -> PartialConsensusConfig (BlockProtocol (SophieBlock (EvieEra c)))
  -> PartialConsensusConfig (BlockProtocol (SophieBlock (JenEra c)))
  -> PartialConsensusConfig (BlockProtocol (SophieBlock (AurumEra c)))
  -> BccConsensusConfig c
pattern BccConsensusConfig cfgCole cfgSophie cfgEvie cfgJen cfgAurum <-
    HardForkConsensusConfig {
        hardForkConsensusConfigPerEra = PerEraConsensusConfig
          (  WrapPartialConsensusConfig cfgCole
          :* WrapPartialConsensusConfig cfgSophie
          :* WrapPartialConsensusConfig cfgEvie
          :* WrapPartialConsensusConfig cfgJen
          :* WrapPartialConsensusConfig cfgAurum
          :* Nil
          )
      }

{-# COMPLETE BccConsensusConfig #-}

{-------------------------------------------------------------------------------
  LedgerConfig
-------------------------------------------------------------------------------}

-- | The 'LedgerConfig' for 'BccBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of the
-- Cole, Sophie, ... 'PartialLedgerConfig's.
--
-- NOTE: not 'LedgerConfig', but 'PartialLedgerConfig'.
type BccLedgerConfig c = HardForkLedgerConfig (BccEras c)

pattern BccLedgerConfig
  :: PartialLedgerConfig ColeBlock
  -> PartialLedgerConfig (SophieBlock (SophieEra c))
  -> PartialLedgerConfig (SophieBlock (EvieEra c))
  -> PartialLedgerConfig (SophieBlock (JenEra c))
  -> PartialLedgerConfig (SophieBlock (AurumEra c))
  -> BccLedgerConfig c
pattern BccLedgerConfig cfgCole cfgSophie cfgEvie cfgJen cfgAurum <-
    HardForkLedgerConfig {
        hardForkLedgerConfigPerEra = PerEraLedgerConfig
          (  WrapPartialLedgerConfig cfgCole
          :* WrapPartialLedgerConfig cfgSophie
          :* WrapPartialLedgerConfig cfgEvie
          :* WrapPartialLedgerConfig cfgJen
          :* WrapPartialLedgerConfig cfgAurum
          :* Nil
          )
      }

{-# COMPLETE BccLedgerConfig #-}

{-------------------------------------------------------------------------------
  LedgerState
-------------------------------------------------------------------------------}

-- | The 'LedgerState' for 'BccBlock'.
--
-- NOTE: the 'BccLedgerState' contains more than just the current era's
-- 'LedgerState'. We don't give access to those internal details through the
-- pattern synonyms. This is also the reason the pattern synonyms are not
-- bidirectional.
type BccLedgerState c = LedgerState (BccBlock c)

pattern LedgerStateCole
  :: LedgerState ColeBlock
  -> BccLedgerState c
pattern LedgerStateCole st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleCole (State.Current { currentState = st })))

pattern LedgerStateSophie
  :: LedgerState (SophieBlock (SophieEra c))
  -> BccLedgerState c
pattern LedgerStateSophie st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleSophie _ (State.Current { currentState = st })))

pattern LedgerStateEvie
  :: LedgerState (SophieBlock (EvieEra c))
  -> BccLedgerState c
pattern LedgerStateEvie st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleEvie _ _  (State.Current { currentState = st })))

pattern LedgerStateJen
  :: LedgerState (SophieBlock (JenEra c))
  -> BccLedgerState c
pattern LedgerStateJen st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleJen _ _ _ (State.Current { currentState = st })))

pattern LedgerStateAurum
  :: LedgerState (SophieBlock (AurumEra c))
  -> BccLedgerState c
pattern LedgerStateAurum st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleAurum _ _ _ _ (State.Current { currentState = st })))

{-# COMPLETE LedgerStateCole
           , LedgerStateSophie
           , LedgerStateEvie
           , LedgerStateJen
           , LedgerStateAurum #-}

{-------------------------------------------------------------------------------
  ChainDepState
-------------------------------------------------------------------------------}

-- | The 'ChainDepState' for 'BccBlock'.
--
-- NOTE: the 'BccChainDepState' contains more than just the current era's
-- 'ChainDepState'. We don't give access to those internal details through the
-- pattern synonyms. This is also the reason the pattern synonyms are not
-- bidirectional.
type BccChainDepState c = HardForkChainDepState (BccEras c)

pattern ChainDepStateCole
  :: ChainDepState (BlockProtocol ColeBlock)
  -> BccChainDepState c
pattern ChainDepStateCole st <-
    State.HardForkState
      (TeleCole (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateSophie
  :: ChainDepState (BlockProtocol (SophieBlock (SophieEra c)))
  -> BccChainDepState c
pattern ChainDepStateSophie st <-
    State.HardForkState
      (TeleSophie _ (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateEvie
  :: ChainDepState (BlockProtocol (SophieBlock (EvieEra c)))
  -> BccChainDepState c
pattern ChainDepStateEvie st <-
    State.HardForkState
      (TeleEvie _ _ (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateJen
  :: ChainDepState (BlockProtocol (SophieBlock (JenEra c)))
  -> BccChainDepState c
pattern ChainDepStateJen st <-
    State.HardForkState
      (TeleJen _ _ _ (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateAurum
  :: ChainDepState (BlockProtocol (SophieBlock (JenEra c)))
  -> BccChainDepState c
pattern ChainDepStateAurum st <-
    State.HardForkState
      (TeleAurum _ _ _ _ (State.Current { currentState = WrapChainDepState st }))

{-# COMPLETE ChainDepStateCole
           , ChainDepStateSophie
           , ChainDepStateEvie
           , ChainDepStateJen
           , ChainDepStateAurum #-}
