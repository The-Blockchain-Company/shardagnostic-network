{-# LANGUAGE NamedFieldPuns #-}

-- Subscription worker for client applications connecting with 'LocalSnocket'
-- which is using either unix sockets or Windows' named pipes.
--
module Shardagnostic.Network.Subscription.Client
  ( ClientSubscriptionParams (..)
  , clientSubscriptionWorker
  ) where

import           Control.Monad.Class.MonadTime
import           Control.Tracer

import           Data.Void (Void)
import           Data.Functor.Identity (Identity (..))

import           Shardagnostic.Network.Snocket ( LocalAddress
                                           , LocalSnocket
                                           , LocalSocket
                                           )
import           Shardagnostic.Network.ErrorPolicy ( ErrorPolicies
                                               , ErrorPolicyTrace
                                               , WithAddr
                                               , completeApplicationTx
                                               )
import           Shardagnostic.Network.Socket (NetworkMutableState (..))
import           Shardagnostic.Network.Subscription.Ip (socketStateChangeTx, mainTx)
import           Shardagnostic.Network.Subscription.Worker
import           Shardagnostic.Network.Subscription.Subscriber


data ClientSubscriptionParams a = ClientSubscriptionParams
  { cspAddress                :: !LocalAddress
  -- ^ unix socket or named pipe address
  , cspConnectionAttemptDelay :: !(Maybe DiffTime)
  -- ^ delay between connection attempts
  , cspErrorPolicies          :: !ErrorPolicies
  -- ^ error policies for subscription worker
  }

-- | Client subscription worker keeps subsribing to the 'LocalAddress' using
-- either unix socket or named pipe.
--
clientSubscriptionWorker
    :: LocalSnocket
    -> Tracer IO (SubscriptionTrace LocalAddress)
    -> Tracer IO (WithAddr LocalAddress ErrorPolicyTrace)
    -> NetworkMutableState LocalAddress
    -> ClientSubscriptionParams a
    -> (LocalSocket -> IO a)
    -> IO Void
clientSubscriptionWorker snocket
                         tracer
                         errorPolicyTracer
                         NetworkMutableState { nmsConnectionTable, nmsPeerStates }
                         ClientSubscriptionParams { cspAddress
                                                  , cspConnectionAttemptDelay
                                                  , cspErrorPolicies
                                                  }
                         k =
    worker tracer
           errorPolicyTracer
           nmsConnectionTable
           nmsPeerStates
           snocket
           WorkerCallbacks
            { wcSocketStateChangeTx   = socketStateChangeTx
            , wcCompleteApplicationTx = completeApplicationTx cspErrorPolicies
            , wcMainTx                = mainTx
            }
           workerParams
           k
  where
    workerParams :: WorkerParams IO Identity LocalAddress
    workerParams = WorkerParams {
        wpLocalAddresses         = Identity cspAddress,
        wpSelectAddress          = \_ (Identity addr) -> Just addr,
        wpConnectionAttemptDelay = const cspConnectionAttemptDelay,
        wpSubscriptionTarget     = pure (listSubscriptionTarget [cspAddress]),
        wpValency                = 1
      }
