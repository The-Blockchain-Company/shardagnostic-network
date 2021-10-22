{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Evie () where

import           Shardagnostic.Consensus.Sophie.Eras
import           Shardagnostic.Consensus.Sophie.Ledger

import           Test.ThreadNet.TxGen (TxGen (..))

-- | Dummy generator until CAD-2119 is done, i.e., the transaction generator in
-- the ledger has been generalised over the eras.
instance TxGen (SophieBlock (EvieEra c)) where

  type TxGenExtra (SophieBlock (EvieEra c)) = ()

  testGenTxs _ _ _ _ _ _ = pure []
