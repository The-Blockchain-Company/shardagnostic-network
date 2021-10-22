{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Shardagnostic.Consensus.HardFork.Combinator.Translation (
    -- * Translate from one era to the next
    EraTranslation (..)
  , trivialEraTranslation
  ) where

import           NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))

import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.TypeFamilyWrappers

import           Shardagnostic.Consensus.HardFork.Combinator.State.Types
import           Shardagnostic.Consensus.HardFork.Combinator.Util.InPairs
                     (InPairs (..), RequiringBoth (..))

{-------------------------------------------------------------------------------
  Translate from one era to the next
-------------------------------------------------------------------------------}

data EraTranslation xs = EraTranslation {
      translateLedgerState   :: InPairs (RequiringBoth WrapLedgerConfig    (Translate LedgerState))       xs
    , translateChainDepState :: InPairs (RequiringBoth WrapConsensusConfig (Translate WrapChainDepState)) xs
    , translateLedgerView    :: InPairs (RequiringBoth WrapLedgerConfig    (TranslateForecast LedgerState WrapLedgerView)) xs
    }
  deriving NoThunks
       via OnlyCheckWhnfNamed "EraTranslation" (EraTranslation xs)

trivialEraTranslation :: EraTranslation '[blk]
trivialEraTranslation = EraTranslation {
      translateLedgerState   = PNil
    , translateLedgerView    = PNil
    , translateChainDepState = PNil
    }
