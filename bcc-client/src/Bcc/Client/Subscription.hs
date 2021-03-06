{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Bcc.Client.Subscription (
    subscribe
  , MuxMode (..)
  , ClientCodecs
  , ConnectionId
  , LocalAddress
  , NodeToClientProtocols (..)
  , BlockNodeToClientVersion
  , MuxPeer (..)
  , MuxTrace
  , RunMiniProtocol (..)
  , WithMuxBearer
  , ControlMessage (..)
  , cChainSyncCodec
  , cStateQueryCodec
  , cTxSubmissionCodec
  ) where

import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadSTM
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.Void (Void)

import           Network.Mux.Trace (MuxTrace, WithMuxBearer)

import           Shardagnostic.Network.Magic (NetworkMagic)
import           Shardagnostic.Network.Mux (MuxMode (..), MuxPeer (..),
                     ShardagnosticApplication, RunMiniProtocol (..),
                     ControlMessage (..))
import           Shardagnostic.Network.NodeToClient (ClientSubscriptionParams (..),
                     ConnectionId, LocalAddress,
                     NetworkClientSubcriptionTracers,
                     NodeToClientProtocols (..),
                     NodeToClientVersionData (NodeToClientVersionData),
                     ncSubscriptionWorker, newNetworkMutableState,
                     versionedNodeToClientProtocols)
import           Shardagnostic.Network.NodeToClient (NodeToClientVersion)
import           Shardagnostic.Network.Protocol.Handshake.Version (Versions,
                   foldMapVersions)
import qualified Shardagnostic.Network.Snocket as Snocket

import           Shardagnostic.Consensus.Block (CodecConfig)
import           Shardagnostic.Consensus.Network.NodeToClient (ClientCodecs,
                     cChainSyncCodec, cStateQueryCodec, cTxSubmissionCodec,
                     clientCodecs)
import           Shardagnostic.Consensus.Node.NetworkProtocolVersion
                     (BlockNodeToClientVersion, supportedNodeToClientVersions)
import           Shardagnostic.Consensus.Node.Run (RunNode)

subscribe ::
     RunNode blk
  => Snocket.LocalSnocket
  -> CodecConfig blk
  -> NetworkMagic
  -> NetworkClientSubcriptionTracers
  -> ClientSubscriptionParams ()
  -> (   NodeToClientVersion
      -> ClientCodecs blk IO
      -> ConnectionId LocalAddress
      -> NodeToClientProtocols 'InitiatorMode BSL.ByteString IO x y)
  -> IO Void
subscribe snocket codecConfig networkMagic tracers subscriptionParams protocols = do
    networkState <- newNetworkMutableState
    ncSubscriptionWorker
      snocket
      tracers
      networkState
      subscriptionParams
      (versionedProtocols codecConfig networkMagic
        (\version codecs connectionId _ ->
            protocols version codecs connectionId))

versionedProtocols ::
     forall blk m appType bytes a b. (MonadST m, RunNode blk)
  => CodecConfig blk
  -> NetworkMagic
  -> (   NodeToClientVersion
      -> ClientCodecs blk m
      -> ConnectionId LocalAddress
      -> STM m ControlMessage
      -> NodeToClientProtocols appType bytes m a b)
     -- ^ callback which receives codecs, connection id and STM action which
     -- can be checked if the networking runtime system requests the protocols
     -- to stop.
     --
     -- TODO: the 'RunOrStop' might not be needed for @node-to-client@, hence
     -- it's not exposed in 'subscribe'. We should provide
     -- 'ShardagnosticClientApplication', which does not include it.
  -> Versions
       NodeToClientVersion
       NodeToClientVersionData
       (ShardagnosticApplication appType LocalAddress bytes m a b)
versionedProtocols codecConfig networkMagic callback =
    foldMapVersions applyVersion $
      Map.toList $ supportedNodeToClientVersions (Proxy @blk)
  where
    applyVersion ::
         (NodeToClientVersion, BlockNodeToClientVersion blk)
      -> Versions
           NodeToClientVersion
           NodeToClientVersionData
           (ShardagnosticApplication appType LocalAddress bytes m a b)
    applyVersion (version, blockVersion) =
      versionedNodeToClientProtocols
        version
        (NodeToClientVersionData networkMagic)
        (callback version (clientCodecs codecConfig blockVersion version))
