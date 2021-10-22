module Main (main) where

import qualified System.Directory as Dir
import           System.IO.Temp
import           Test.Tasty

import qualified Test.Shardagnostic.Storage

main :: IO ()
main = do
  sysTmpDir <- Dir.getTemporaryDirectory
  withTempDirectory sysTmpDir "bcc-s-m" $ \tmpDir ->
    defaultMain (tests tmpDir)

tests :: FilePath -> TestTree
tests tmpDir =
  testGroup "shardagnostic-storage"
  [ Test.Shardagnostic.Storage.tests tmpDir
  ]
