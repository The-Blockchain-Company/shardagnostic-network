module Test.Shardagnostic.Storage.ImmutableDB (tests) where

import           Test.Tasty (TestTree, testGroup)

import qualified Test.Shardagnostic.Storage.ImmutableDB.Primary as Primary
import qualified Test.Shardagnostic.Storage.ImmutableDB.StateMachine as StateMachine

{------------------------------------------------------------------------------
  The list of all tests
------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "ImmutableDB" [
      Primary.tests
    , StateMachine.tests
    ]
