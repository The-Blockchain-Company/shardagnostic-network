module Main (main) where

import           System.IO (BufferMode (LineBuffering), hSetBuffering,
                     hSetEncoding, stdout, utf8)

import           Bcc.Crypto.Libsodium (sodiumInit)

import           Test.Tasty
import           Test.Util.Nightly

import qualified Test.Consensus.Bcc.ColeCompatibility (tests)
import qualified Test.Consensus.Bcc.Golden (tests)
import qualified Test.Consensus.Bcc.Serialisation (tests)
import qualified Test.ThreadNet.EvieJen (tests)
import qualified Test.ThreadNet.Bcc (tests)
import qualified Test.ThreadNet.JenAurum (tests)
import qualified Test.ThreadNet.SophieEvie (tests)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  sodiumInit
  defaultMainWithBcccoinNightly tests

tests :: TestTree
tests =
  testGroup "bcc"
  [ Test.Consensus.Bcc.ColeCompatibility.tests
  , Test.Consensus.Bcc.Golden.tests
  , Test.Consensus.Bcc.Serialisation.tests
  , Test.ThreadNet.EvieJen.tests
  , Test.ThreadNet.Bcc.tests
  , Test.ThreadNet.JenAurum.tests
  , Test.ThreadNet.SophieEvie.tests
  ]
