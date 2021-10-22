{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}

module Shardagnostic.Network.Point
  ( WithOrigin (..)
  , Block (..)
  , origin
  , at
  , block
  , fromWithOrigin
  , withOrigin
  , withOriginToMaybe
  , withOriginFromMaybe
  ) where

import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Bcc.Slotting.Slot

data Block slot hash = Block
  { blockPointSlot :: !slot
  , blockPointHash :: !hash
  }
  deriving (Eq, Ord, Show, Generic, NoThunks)

block :: slot -> hash -> WithOrigin (Block slot hash)
block slot hash = at (Block slot hash)
