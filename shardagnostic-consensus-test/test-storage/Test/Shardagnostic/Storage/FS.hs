module Test.Shardagnostic.Storage.FS (tests) where

import qualified Test.Shardagnostic.Storage.FS.StateMachine as StateMachine
import           Test.Tasty (TestTree, testGroup)

tests :: FilePath -> TestTree
tests tmpDir = testGroup "HasFS" [
      StateMachine.tests tmpDir
    ]
