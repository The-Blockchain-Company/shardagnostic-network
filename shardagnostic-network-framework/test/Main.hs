module Main (main) where

import           Test.Tasty

import qualified Test.Network.TypedProtocol.PingPong.Codec as PingPong
import qualified Test.Network.TypedProtocol.ReqResp.Codec as ReqResp
import qualified Test.Shardagnostic.Network.Driver as Driver
import qualified Test.Shardagnostic.Network.Socket as Socket
import qualified Test.Shardagnostic.Network.Subscription as Subscription
import qualified Test.Shardagnostic.Network.RateLimiting as RateLimiting

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "shardagnostic-network-framework"
  [ PingPong.tests
  , ReqResp.tests
  , Driver.tests
  , Socket.tests
  , Subscription.tests
  , RateLimiting.tests
  ]


