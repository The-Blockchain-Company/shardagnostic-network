{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Cole () where

import           Shardagnostic.Consensus.Cole.Ledger

import           Test.ThreadNet.TxGen

instance TxGen ColeBlock where
  -- We don't generate transactions for 'ColeBlock', but we do for
  -- 'DualColeBlock'.
  testGenTxs _ _ _ _ _ _ = return []
