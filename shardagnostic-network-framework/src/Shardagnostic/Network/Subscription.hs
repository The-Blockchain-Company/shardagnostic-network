-- | Public interface of 'Shardagnostic.Network.Subscription' workers.
--
module Shardagnostic.Network.Subscription
    ( -- * IP Subscription Worker
      ipSubscriptionWorker
    , IPSubscriptionTarget (..)
      -- * DNS Subscription Worker
    , dnsSubscriptionWorker
    , DnsSubscriptionTarget (..)
    , ConnectResult (..)


      -- * Constants
    , defaultConnectionAttemptDelay
    , minConnectionAttemptDelay
    , maxConnectionAttemptDelay
    , ipRetryDelay
    , resolutionDelay

      -- * Errors
    , SubscriberError (..)

      -- * Tracing
    , SubscriptionTrace (..)
    , WithIPList (..)
    , DnsTrace (..)
    , WithDomainName (..)
    ) where

import           Shardagnostic.Network.Subscription.Ip
import           Shardagnostic.Network.Subscription.Dns
import           Shardagnostic.Network.Subscription.Worker
