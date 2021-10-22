{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Aurum () where

import           Shardagnostic.Consensus.Sophie.Eras
import           Shardagnostic.Consensus.Sophie.Ledger

import           Test.ThreadNet.TxGen (TxGen (..))

-- | Dummy generator until CAD-2119 is done, i.e., the transaction generator in
-- the ledger has been generalised over the eras.
instance TxGen (SophieBlock (AurumEra c)) where

  type TxGenExtra (SophieBlock (AurumEra c)) = ()

  testGenTxs _ _ _ _ _ _ = pure []
