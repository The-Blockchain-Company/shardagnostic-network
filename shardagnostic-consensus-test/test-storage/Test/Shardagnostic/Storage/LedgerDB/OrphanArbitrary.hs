{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Shardagnostic.Storage.LedgerDB.OrphanArbitrary () where

import           Shardagnostic.Consensus.Config.SecurityParam (SecurityParam (..))
import           Test.QuickCheck

{-------------------------------------------------------------------------------
  Orphan Arbitrary instances
-------------------------------------------------------------------------------}

instance Arbitrary SecurityParam where
  arbitrary = SecurityParam <$> choose (0, 6)
  shrink (SecurityParam k) = SecurityParam <$> shrink k
