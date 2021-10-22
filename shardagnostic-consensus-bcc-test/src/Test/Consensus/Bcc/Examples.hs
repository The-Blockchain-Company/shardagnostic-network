{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Bcc.Examples (
    -- * Setup
    codecConfig
    -- * Examples
  , exampleApplyTxErrWrongEraCole
  , exampleApplyTxErrWrongEraSophie
  , exampleEraMismatchCole
  , exampleEraMismatchSophie
  , exampleQueryAnytimeSophie
  , exampleQueryEraMismatchCole
  , exampleQueryEraMismatchSophie
  , exampleResultAnytimeSophie
  , exampleResultEraMismatchCole
  , exampleResultEraMismatchSophie
  , examples
  ) where

import           Data.Coerce (Coercible)
import           Data.SOP.Strict

import           Shardagnostic.Network.Block (Serialised (..))

import           Shardagnostic.Consensus.Block
import qualified Shardagnostic.Consensus.HardFork.History as History
import           Shardagnostic.Consensus.HeaderValidation (AnnTip)
import           Shardagnostic.Consensus.Ledger.Extended (ExtLedgerState (..))
import           Shardagnostic.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import           Shardagnostic.Consensus.Storage.Serialisation
import           Shardagnostic.Consensus.TypeFamilyWrappers
import           Shardagnostic.Consensus.Util.Counting (Exactly (..))
import           Shardagnostic.Consensus.Util.SOP (Index (..))

import           Shardagnostic.Consensus.HardFork.Combinator
import           Shardagnostic.Consensus.HardFork.Combinator.Embed.Nary
import qualified Shardagnostic.Consensus.HardFork.Combinator.State as State

import           Shardagnostic.Consensus.Cole.Ledger (ColeBlock)
import qualified Shardagnostic.Consensus.Cole.Ledger as Cole

import           Shardagnostic.Consensus.Sophie.Ledger (SophieBlock)
import qualified Shardagnostic.Consensus.Sophie.Ledger as Sophie

import           Shardagnostic.Consensus.Bcc.Block
import           Shardagnostic.Consensus.Bcc.CanHardFork ()

import           Test.Util.Serialisation.Golden (Examples, Labelled, labelled)
import qualified Test.Util.Serialisation.Golden as Golden
import           Test.Util.Serialisation.Roundtrip (SomeResult (..))

import qualified Test.Consensus.Cole.Examples as Cole

import qualified Test.Consensus.Sophie.Examples as Sophie

type Crypto = StandardCrypto

eraExamples :: NP Examples (BccEras Crypto)
eraExamples =
       Cole.examples
    :* Sophie.examplesSophie
    :* Sophie.examplesEvie
    :* Sophie.examplesJen
    :* Sophie.examplesAurum
    :* Nil

-- | By using this function, we can't forget to update this test when adding a
-- new era to 'BccEras'.
combineEras ::
     NP Examples (BccEras Crypto)
  -> Examples (BccBlock Crypto)
combineEras = mconcat . hcollapse . hap eraInjections
  where
    eraInjections :: NP (Examples -.-> K (Examples (BccBlock Crypto)))
                        (BccEras Crypto)
    eraInjections =
           fn (K . injExamples "Cole"   IZ)
        :* fn (K . injExamples "Sophie" (IS IZ))
        :* fn (K . injExamples "Evie" (IS (IS IZ)))
        :* fn (K . injExamples "Jen"    (IS (IS (IS IZ))))
        :* fn (K . injExamples "Aurum"  (IS (IS (IS (IS IZ)))))
        :* Nil

    injExamples ::
         String
      -> Index (BccEras Crypto) blk
      -> Examples blk
      -> Examples (BccBlock Crypto)
    injExamples eraName idx =
          Golden.prefixExamples eraName
        . inject exampleStartBounds idx

{-------------------------------------------------------------------------------
  Inject instances
-------------------------------------------------------------------------------}

-- | In reality, an era tag would be prepended, but we're testing that the
-- encoder doesn't care what the bytes are.
instance Inject Serialised where
  inject _ _ (Serialised _) = Serialised "<BCC_BLOCK>"

instance Inject SomeResult where
  inject _ idx (SomeResult q r) =
      SomeResult (QueryIfCurrent (injectQuery idx q)) (Right r)

instance Inject Examples where
  inject startBounds (idx :: Index xs x) Golden.Examples {..} = Golden.Examples {
        exampleBlock            = inj (Proxy @I)                       exampleBlock
      , exampleSerialisedBlock  = inj (Proxy @Serialised)              exampleSerialisedBlock
      , exampleHeader           = inj (Proxy @Header)                  exampleHeader
      , exampleSerialisedHeader = inj (Proxy @SerialisedHeader)        exampleSerialisedHeader
      , exampleHeaderHash       = inj (Proxy @WrapHeaderHash)          exampleHeaderHash
      , exampleGenTx            = inj (Proxy @GenTx)                   exampleGenTx
      , exampleGenTxId          = inj (Proxy @WrapGenTxId)             exampleGenTxId
      , exampleApplyTxErr       = inj (Proxy @WrapApplyTxErr)          exampleApplyTxErr
      , exampleQuery            = inj (Proxy @(SomeSecond BlockQuery)) exampleQuery
      , exampleResult           = inj (Proxy @SomeResult)              exampleResult
      , exampleAnnTip           = inj (Proxy @AnnTip)                  exampleAnnTip
      , exampleLedgerState      = inj (Proxy @LedgerState)             exampleLedgerState
      , exampleChainDepState    = inj (Proxy @WrapChainDepState)       exampleChainDepState
      , exampleExtLedgerState   = inj (Proxy @ExtLedgerState)          exampleExtLedgerState
      }
    where
      inj ::
           forall f a b.
           ( Inject f
           , Coercible a (f x)
           , Coercible b (f (HardForkBlock xs))
           )
        => Proxy f -> Labelled a -> Labelled b
      inj p = fmap (fmap (inject' p startBounds idx))

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

coleEraParams :: History.EraParams
coleEraParams = Cole.coleEraParams Cole.ledgerConfig

sophieEraParams :: History.EraParams
sophieEraParams = Sophie.sophieEraParams Sophie.testSophieGenesis

evieEraParams :: History.EraParams
evieEraParams = Sophie.sophieEraParams Sophie.testSophieGenesis

maryEraParams :: History.EraParams
maryEraParams = Sophie.sophieEraParams Sophie.testSophieGenesis

aurumEraParams :: History.EraParams
aurumEraParams = Sophie.sophieEraParams Sophie.testSophieGenesis

-- | We use 10, 20, 30, 40, ... as the transition epochs
sophieTransitionEpoch :: EpochNo
sophieTransitionEpoch = 10

coleStartBound :: History.Bound
coleStartBound = History.initBound

sophieStartBound :: History.Bound
sophieStartBound =
    History.mkUpperBound
      coleEraParams
      coleStartBound
      sophieTransitionEpoch

evieStartBound :: History.Bound
evieStartBound =
    History.mkUpperBound
      sophieEraParams
      sophieStartBound
      20

jenStartBound :: History.Bound
jenStartBound =
    History.mkUpperBound
      evieEraParams
      evieStartBound
      30

aurumStartBound :: History.Bound
aurumStartBound =
    History.mkUpperBound
      maryEraParams
      jenStartBound
      40

exampleStartBounds :: Exactly (BccEras Crypto) History.Bound
exampleStartBounds = Exactly $
    (  K coleStartBound
    :* K sophieStartBound
    :* K evieStartBound
    :* K jenStartBound
    :* K aurumStartBound
    :* Nil
    )

bccShape :: History.Shape (BccEras Crypto)
bccShape = History.Shape $ Exactly $
    (  K coleEraParams
    :* K sophieEraParams
    :* K evieEraParams
    :* K maryEraParams
    :* K aurumEraParams
    :* Nil
    )

summary :: History.Summary (BccEras Crypto)
summary =
    State.reconstructSummary
      bccShape
      (State.TransitionKnown sophieTransitionEpoch)
      (hardForkLedgerStatePerEra (ledgerStateCole coleLedger))
  where
    (_, coleLedger) = head $ Golden.exampleLedgerState Cole.examples

eraInfoCole :: SingleEraInfo ColeBlock
eraInfoCole = singleEraInfo (Proxy @ColeBlock)

eraInfoSophie :: SingleEraInfo (SophieBlock StandardSophie)
eraInfoSophie = singleEraInfo (Proxy @(SophieBlock StandardSophie))

codecConfig :: BccCodecConfig Crypto
codecConfig =
    BccCodecConfig
      Cole.codecConfig
      Sophie.SophieCodecConfig
      Sophie.SophieCodecConfig
      Sophie.SophieCodecConfig
      Sophie.SophieCodecConfig

ledgerStateCole ::
     LedgerState ColeBlock
  -> LedgerState (BccBlock Crypto)
ledgerStateCole stCole =
    HardForkLedgerState $ HardForkState $ TZ cur
  where
    cur = State.Current {
          currentStart = History.initBound
        , currentState = stCole
        }

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

-- | Multi-era examples, e.g., applying a transaction to the wrong era.
multiEraExamples :: Examples (BccBlock Crypto)
multiEraExamples = mempty {
      Golden.exampleApplyTxErr = labelled [
          ("WrongEraCole",   exampleApplyTxErrWrongEraCole)
        , ("WrongEraSophie", exampleApplyTxErrWrongEraSophie)
        ]
    , Golden.exampleQuery = labelled [
          ("AnytimeCole",   exampleQueryAnytimeCole)
        , ("AnytimeSophie", exampleQueryAnytimeSophie)
        , ("HardFork",       exampleQueryHardFork)
        ]
    , Golden.exampleResult = labelled [
          ("EraMismatchCole",   exampleResultEraMismatchCole)
        , ("EraMismatchSophie", exampleResultEraMismatchSophie)
        , ("AnytimeCole",       exampleResultAnytimeCole)
        , ("AnytimeSophie",     exampleResultAnytimeSophie)
        , ("HardFork",           exampleResultHardFork)
        ]
    }

-- | The examples: the examples from each individual era lifted in to
-- 'BccBlock' /and/ the multi-era examples.
examples :: Examples (BccBlock Crypto)
examples = combineEras eraExamples <> multiEraExamples

-- | Applying a Sophie thing to a Cole ledger
exampleEraMismatchCole :: MismatchEraInfo (BccEras Crypto)
exampleEraMismatchCole =
    MismatchEraInfo $ MR (Z eraInfoSophie) (LedgerEraInfo eraInfoCole)

-- | Applying a Cole thing to a Sophie ledger
exampleEraMismatchSophie :: MismatchEraInfo (BccEras Crypto)
exampleEraMismatchSophie =
    MismatchEraInfo $ ML eraInfoCole (Z (LedgerEraInfo eraInfoSophie))

exampleApplyTxErrWrongEraCole :: ApplyTxErr (BccBlock Crypto)
exampleApplyTxErrWrongEraCole =
      HardForkApplyTxErrWrongEra exampleEraMismatchCole

exampleApplyTxErrWrongEraSophie :: ApplyTxErr (BccBlock Crypto)
exampleApplyTxErrWrongEraSophie =
      HardForkApplyTxErrWrongEra exampleEraMismatchSophie

exampleQueryEraMismatchCole :: SomeSecond BlockQuery (BccBlock Crypto)
exampleQueryEraMismatchCole =
    SomeSecond (QueryIfCurrentSophie Sophie.GetLedgerTip)

exampleQueryEraMismatchSophie :: SomeSecond BlockQuery (BccBlock Crypto)
exampleQueryEraMismatchSophie =
    SomeSecond (QueryIfCurrentCole Cole.GetUpdateInterfaceState)

exampleQueryAnytimeCole :: SomeSecond BlockQuery (BccBlock Crypto)
exampleQueryAnytimeCole =
    SomeSecond (QueryAnytimeCole GetEraStart)

exampleQueryAnytimeSophie :: SomeSecond BlockQuery (BccBlock Crypto)
exampleQueryAnytimeSophie =
    SomeSecond (QueryAnytimeSophie GetEraStart)

exampleQueryHardFork :: SomeSecond BlockQuery (BccBlock Crypto)
exampleQueryHardFork =
    SomeSecond (QueryHardFork GetInterpreter)

exampleResultEraMismatchCole :: SomeResult (BccBlock Crypto)
exampleResultEraMismatchCole =
    SomeResult
      (QueryIfCurrentSophie Sophie.GetLedgerTip)
      (Left exampleEraMismatchCole)

exampleResultEraMismatchSophie :: SomeResult (BccBlock Crypto)
exampleResultEraMismatchSophie =
    SomeResult
      (QueryIfCurrentCole Cole.GetUpdateInterfaceState)
      (Left exampleEraMismatchSophie)

exampleResultAnytimeCole :: SomeResult (BccBlock Crypto)
exampleResultAnytimeCole =
    SomeResult (QueryAnytimeCole GetEraStart) (Just coleStartBound)

exampleResultAnytimeSophie :: SomeResult (BccBlock Crypto)
exampleResultAnytimeSophie =
    SomeResult (QueryAnytimeSophie GetEraStart) (Just sophieStartBound)

exampleResultHardFork :: SomeResult (BccBlock Crypto)
exampleResultHardFork =
    SomeResult (QueryHardFork GetInterpreter) (History.mkInterpreter summary)
