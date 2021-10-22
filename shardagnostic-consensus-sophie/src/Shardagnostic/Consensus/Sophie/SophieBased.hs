{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Shardagnostic.Consensus.Sophie.SophieBased (
    -- * Injection from Sophie-based eras into the Bcc eras
    InjectSophie
  , injectSophieNP
  , injectSophieOptNP
    -- * Transform Sophie-based types
  , EraCrypto
  , HasCrypto
  ) where

import           Data.SOP.Strict

import           Shardagnostic.Consensus.Util.OptNP (OptNP (..))

import           Shardagnostic.Consensus.Sophie.Eras (EraCrypto)
import           Shardagnostic.Consensus.Sophie.Ledger (SophieBlock)

{-------------------------------------------------------------------------------
  Injection from Sophie-based eras into consensus mode eras
-------------------------------------------------------------------------------}

-- | Witness the relation between consensus mode (e.g. Bcc) eras and the Sophie-based eras.
class    consensusModeEra ~ SophieBlock sophieEra => InjectSophie sophieEra consensusModeEra
instance consensusModeEra ~ SophieBlock sophieEra => InjectSophie sophieEra consensusModeEra

injectSophieNP ::
     AllZip InjectSophie sophieEras consensusModeEras
  => (   forall sophieEra consensusModeEra.
         InjectSophie sophieEra consensusModeEra
      => f sophieEra -> g consensusModeEra
     )
  -> NP f sophieEras -> NP g consensusModeEras
injectSophieNP _ Nil       = Nil
injectSophieNP f (x :* xs) = f x :* injectSophieNP f xs

injectSophieOptNP ::
     AllZip InjectSophie sophieEras consensusModeEras
  => (   forall sophieEra consensusModeEra.
         InjectSophie sophieEra consensusModeEra
      => f sophieEra -> g consensusModeEra
     )
  -> OptNP empty f sophieEras -> OptNP empty g consensusModeEras
injectSophieOptNP _ OptNil         = OptNil
injectSophieOptNP f (OptSkip   xs) = OptSkip (injectSophieOptNP f xs)
injectSophieOptNP f (OptCons x xs) = OptCons (f x) (injectSophieOptNP f xs)

{-------------------------------------------------------------------------------
  Transform Sophie-based types
-------------------------------------------------------------------------------}

-- | Witness the relation between the crypto used by a Sophie-based era.
--
-- Can be partially applied while an equality constraint cannot.
class EraCrypto era ~ c => HasCrypto c era
instance EraCrypto era ~ c => HasCrypto c era
