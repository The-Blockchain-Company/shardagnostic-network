{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Shardagnostic.Consensus.HardFork.Combinator.Abstract.CanHardFork (CanHardFork (..)) where

import           Data.SOP.Strict
import           Data.Typeable

import           Shardagnostic.Consensus.TypeFamilyWrappers
import           Shardagnostic.Consensus.Util.SOP

import           Shardagnostic.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import           Shardagnostic.Consensus.HardFork.Combinator.InjectTxs
import           Shardagnostic.Consensus.HardFork.Combinator.Protocol.ChainSel
import           Shardagnostic.Consensus.HardFork.Combinator.Translation
import           Shardagnostic.Consensus.HardFork.Combinator.Util.Functors
                     (Product2)
import           Shardagnostic.Consensus.HardFork.Combinator.Util.InPairs (InPairs,
                     RequiringBoth)
import qualified Shardagnostic.Consensus.HardFork.Combinator.Util.InPairs as InPairs
import           Shardagnostic.Consensus.HardFork.Combinator.Util.Tails (Tails)
import qualified Shardagnostic.Consensus.HardFork.Combinator.Util.Tails as Tails

{-------------------------------------------------------------------------------
  CanHardFork
-------------------------------------------------------------------------------}

class (All SingleEraBlock xs, Typeable xs, IsNonEmpty xs) => CanHardFork xs where
  hardForkEraTranslation :: EraTranslation xs
  hardForkChainSel       :: Tails AcrossEraSelection xs
  hardForkInjectTxs      ::
    InPairs
      ( RequiringBoth
          WrapLedgerConfig
          (Product2 InjectTx InjectValidatedTx)
      )
      xs

instance SingleEraBlock blk => CanHardFork '[blk] where
  hardForkEraTranslation = trivialEraTranslation
  hardForkChainSel       = Tails.mk1
  hardForkInjectTxs      = InPairs.mk1
