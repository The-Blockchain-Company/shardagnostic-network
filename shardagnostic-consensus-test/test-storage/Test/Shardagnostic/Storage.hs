{-# LANGUAGE CPP #-}
module Test.Shardagnostic.Storage (tests) where

import qualified Test.Shardagnostic.Storage.ChainDB as ChainDB
import qualified Test.Shardagnostic.Storage.FS as FS
import qualified Test.Shardagnostic.Storage.ImmutableDB as ImmutableDB
import qualified Test.Shardagnostic.Storage.LedgerDB as LedgerDB
import qualified Test.Shardagnostic.Storage.VolatileDB as VolatileDB
import           Test.Tasty (TestTree, testGroup)

--
-- The list of all tests
--

tests :: FilePath -> TestTree
tests tmpDir = testGroup "Storage" $
    -- The FS tests fail for darwin on CI, see #352. So disable them for now.
    [ FS.tests tmpDir | not darwin ] <>
    [ ImmutableDB.tests
    , VolatileDB.tests
    , LedgerDB.tests
    , ChainDB.tests
    ]

darwin :: Bool
#ifdef darwin_HOST_OS
darwin = True
#else
darwin = False
#endif
