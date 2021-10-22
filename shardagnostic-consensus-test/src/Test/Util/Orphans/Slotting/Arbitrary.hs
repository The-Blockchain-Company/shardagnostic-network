{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Placed here to separate them from the other orphan instances due to a
-- conflict with other instances in bcc-ledger-specs.
module Test.Util.Orphans.Slotting.Arbitrary () where

import           Data.Word
import           Test.QuickCheck

import           Bcc.Slotting.Slot

deriving via Word64 instance Arbitrary SlotNo
deriving via Word64 instance Arbitrary EpochNo
