{-# LANGUAGE DeriveGeneric #-}

module Shardagnostic.Network.PeerSelection.Types (
    PeerSource(..),
    PeerAdvertise(..),
    PeerStatus(..),
  ) where

import           GHC.Generics (Generic)


-- | Where did this peer come from? Policy functions can choose to treat
-- peers differently depending on where we found them from.
--
data PeerSource = PeerSourceLocalRoot
                | PeerSourcePublicRoot
                | PeerSourceGossip
                | PeerSourceStaleRoot
--              | PeerSource -- it requested us to advertise it
  deriving (Eq, Ord, Show, Enum)


-- | Should this peer be advertised to other peers asking for known peers?
-- For certain peers specified by configuration it would be an appropriate
-- policy to keep them private.
--
data PeerAdvertise = DoAdvertisePeer
                   | DoNotAdvertisePeer
  deriving (Eq, Show, Generic)


data PeerStatus =
       PeerCold
     | PeerWarm
     | PeerHot
  deriving (Eq, Ord, Show)

