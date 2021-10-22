module Main (main) where

import           Test.Tasty

import qualified Test.Consensus.Cole.Golden (tests)
import qualified Test.Consensus.Cole.Serialisation (tests)
import qualified Test.ThreadNet.Cole (tests)
import qualified Test.ThreadNet.DualCole (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "cole"
  [ Test.Consensus.Cole.Golden.tests
  , Test.Consensus.Cole.Serialisation.tests
  , Test.ThreadNet.Cole.tests
  , Test.ThreadNet.DualCole.tests
  ]
