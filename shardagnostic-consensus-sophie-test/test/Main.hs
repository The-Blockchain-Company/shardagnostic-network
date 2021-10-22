module Main (main) where

import           Bcc.Crypto.Libsodium (sodiumInit)

import           Test.Tasty
import           Test.Util.Nightly

import qualified Test.Consensus.Sophie.Coherence (tests)
import qualified Test.Consensus.Sophie.Golden (tests)
import qualified Test.Consensus.Sophie.Serialisation (tests)
import qualified Test.ThreadNet.Sophie (tests)

main :: IO ()
main = sodiumInit >> defaultMainWithBcccoinNightly tests

tests :: TestTree
tests =
  testGroup "sophie"
  [ Test.Consensus.Sophie.Coherence.tests
  , Test.Consensus.Sophie.Golden.tests
  , Test.Consensus.Sophie.Serialisation.tests
  , Test.ThreadNet.Sophie.tests
  ]
