module Test.Shardagnostic.Storage.VolatileDB (tests) where

import           Test.Tasty (TestTree, testGroup)

import qualified Test.Shardagnostic.Storage.VolatileDB.StateMachine as StateMachine


tests :: TestTree
tests = testGroup "VolatileDB"
    [ StateMachine.tests
    ]
