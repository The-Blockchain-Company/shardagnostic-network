{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Shardagnostic.Consensus.Util.Orphans () where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.Serialise (Serialise (..))
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as PSQ
import           Data.SOP.Strict
import           NoThunks.Class (NoThunks (..), OnlyCheckWhnfNamed (..),
                     allNoThunks, noThunksInKeysAndValues)

import           Control.Tracer (Tracer)

import           Control.Monad.Class.MonadTime (Time (..))

import           Shardagnostic.Consensus.Util.MonadSTM.NormalForm

import           Bcc.Crypto.DSIGN.Class
import           Bcc.Crypto.DSIGN.Mock (MockDSIGN)
import           Bcc.Crypto.Hash (Hash)

import           Shardagnostic.Network.AnchoredFragment (AnchoredFragment)
import qualified Shardagnostic.Network.AnchoredFragment as AF
import           Shardagnostic.Network.MockChain.Chain (Chain (..))

import           Shardagnostic.Consensus.Block.Abstract
import           Shardagnostic.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance Condense (HeaderHash block) => Condense (Point block) where
    condense GenesisPoint     = "Origin"
    condense (BlockPoint s h) = "(Point " <> condense s <> ", " <> condense h <> ")"

instance Condense block => Condense (Chain block) where
    condense Genesis   = "Genesis"
    condense (cs :> b) = condense cs <> " :> " <> condense b

instance (Condense block, HasHeader block, Condense (HeaderHash block))
    => Condense (AnchoredFragment block) where
    condense (AF.Empty pt) = "EmptyAnchor " <> condense (AF.anchorToPoint pt)
    condense (cs AF.:> b)  = condense cs <> " :> " <> condense b

{-------------------------------------------------------------------------------
  Serialise
-------------------------------------------------------------------------------}

instance Serialise (Hash h a) where

instance Serialise (VerKeyDSIGN MockDSIGN) where
  encode = encodeVerKeyDSIGN
  decode = decodeVerKeyDSIGN

{-------------------------------------------------------------------------------
  NoThunks
-------------------------------------------------------------------------------}

instance NoThunks a => NoThunks (StrictTVar IO a) where
  showTypeOf _ = "StrictTVar IO"
  wNoThunks ctxt tv = do
      -- We can't use @atomically $ readTVar ..@ here, as that will lead to a
      -- "Control.Concurrent.STM.atomically was nested" exception.
      a <- readTVarIO tv
      noThunks ctxt a

instance (NoThunks k, NoThunks v)
      => NoThunks (Bimap k v) where
  wNoThunks ctxt = noThunksInKeysAndValues ctxt . Bimap.toList

instance ( NoThunks p
         , NoThunks v
         , Ord p
         ) => NoThunks (IntPSQ p v) where
  showTypeOf _ = "IntPSQ"
  wNoThunks ctxt =
        allNoThunks
      . concatMap (\(k, p, v) ->
        [ noThunks ctxt k
        , noThunks ctxt p
        , noThunks ctxt v])
      . PSQ.toList

deriving via OnlyCheckWhnfNamed "Decoder" (Decoder s a) instance NoThunks (Decoder s a)

deriving via OnlyCheckWhnfNamed "Tracer" (Tracer m ev) instance NoThunks (Tracer m ev)

deriving newtype instance NoThunks Time

instance NoThunks a => NoThunks (K a b) where
  showTypeOf _ = showTypeOf (Proxy @a)
  wNoThunks ctxt (K a) = wNoThunks ("K":ctxt) a
