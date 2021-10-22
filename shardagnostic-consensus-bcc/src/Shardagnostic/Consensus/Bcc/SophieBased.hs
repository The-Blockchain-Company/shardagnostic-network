{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Shardagnostic.Consensus.Bcc.SophieBased (overSophieBasedLedgerState) where

import           Data.SOP.Strict

import           Shardagnostic.Consensus.HardFork.Combinator

import           Shardagnostic.Consensus.Bcc.Block
import           Shardagnostic.Consensus.Sophie.Ledger (SophieBlock)
import           Shardagnostic.Consensus.Sophie.Protocol (OptimumCrypto)
import           Shardagnostic.Consensus.Sophie.SophieBased

-- | When the given ledger state corresponds to a Sophie-based era, apply the
-- given function to it.
overSophieBasedLedgerState ::
     forall c. OptimumCrypto c
  => (   forall era. (EraCrypto era ~ c, SophieBasedEra era)
      => LedgerState (SophieBlock era)
      -> LedgerState (SophieBlock era)
     )
  -> LedgerState (BccBlock c)
  -> LedgerState (BccBlock c)
overSophieBasedLedgerState f (HardForkLedgerState st) =
    HardForkLedgerState $ hap fs st
  where
    fs :: NP (LedgerState -.-> LedgerState)
             (BccEras c)
    fs = fn id
        :* injectSophieNP
             reassoc
             (hcpure
               (Proxy @(And (HasCrypto c) SophieBasedEra))
               (fn (Comp . f . unComp)))

    reassoc ::
         (     LedgerState :.: SophieBlock
          -.-> LedgerState :.: SophieBlock
         ) sophieEra
      -> (     LedgerState
          -.-> LedgerState
         ) (SophieBlock sophieEra)
    reassoc g = fn $ unComp . apFn g . Comp
