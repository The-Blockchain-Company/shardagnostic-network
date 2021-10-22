{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Shardagnostic.Consensus.Bcc.Condense () where

import           Shardagnostic.Consensus.HardFork.Combinator.Condense

import           Shardagnostic.Consensus.Cole.Ledger

import           Shardagnostic.Consensus.Sophie.Ledger

import           Shardagnostic.Consensus.Bcc.Block
import           Shardagnostic.Consensus.Bcc.CanHardFork

{-------------------------------------------------------------------------------
  Condense

  TODO where to put this?
-------------------------------------------------------------------------------}

instance CondenseConstraints ColeBlock

instance SophieBasedEra era => CondenseConstraints (SophieBlock era)

instance BccHardForkConstraints c => CondenseConstraints (BccBlock c)
