module Main (main) where

import           Test.Tasty

import qualified Test.Consensus.Ledger.Mock (tests)
import qualified Test.ThreadNet.BFT (tests)
import qualified Test.ThreadNet.LeaderSchedule (tests)
import qualified Test.ThreadNet.PBFT (tests)
import qualified Test.ThreadNet.Optimum (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "shardagnostic-consensus"
  [ Test.Consensus.Ledger.Mock.tests
  , Test.ThreadNet.BFT.tests
  , Test.ThreadNet.LeaderSchedule.tests
  , Test.ThreadNet.PBFT.tests
  , Test.ThreadNet.Optimum.tests
  ]
