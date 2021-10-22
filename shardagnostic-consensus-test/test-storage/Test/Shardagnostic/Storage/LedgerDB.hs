module Test.Shardagnostic.Storage.LedgerDB (tests) where

import           Test.Tasty

import qualified Test.Shardagnostic.Storage.LedgerDB.DiskPolicy as DiskPolicy
import qualified Test.Shardagnostic.Storage.LedgerDB.InMemory as InMemory
import qualified Test.Shardagnostic.Storage.LedgerDB.OnDisk as OnDisk

tests :: TestTree
tests = testGroup "LedgerDB" [
      InMemory.tests
    , OnDisk.tests
    , DiskPolicy.tests
    ]
