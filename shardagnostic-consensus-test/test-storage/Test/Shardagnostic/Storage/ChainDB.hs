module Test.Shardagnostic.Storage.ChainDB (tests) where

import           Test.Tasty

import qualified Test.Shardagnostic.Storage.ChainDB.GcSchedule as GcSchedule
import qualified Test.Shardagnostic.Storage.ChainDB.Iterator as Iterator
import qualified Test.Shardagnostic.Storage.ChainDB.Model.Test as Model
import qualified Test.Shardagnostic.Storage.ChainDB.Paths as Paths
import qualified Test.Shardagnostic.Storage.ChainDB.StateMachine as StateMachine

tests :: TestTree
tests = testGroup "ChainDB" [
      Iterator.tests
    , GcSchedule.tests
    , Model.tests
    , Paths.tests
    , StateMachine.tests
    ]
