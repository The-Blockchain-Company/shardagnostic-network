module Main (main) where

import           Test.Tasty

import qualified Test.AnchoredFragment (tests)
import qualified Test.ChainGenerators (tests)
import qualified Test.Chain (tests)
import qualified Test.ChainProducerState (tests)
import qualified Test.LedgerPeers (tests)
import qualified Test.Pipe (tests)
import qualified Test.PeerState (tests)
import qualified Test.Version (tests)
import qualified Test.Shardagnostic.Network.MockNode (tests)
import qualified Test.Shardagnostic.Network.BlockFetch (tests)
import qualified Test.Shardagnostic.Network.KeepAlive (tests)
import qualified Test.Shardagnostic.Network.NodeToNode.Version (tests)
import qualified Test.Shardagnostic.Network.NodeToClient.Version (tests)
import qualified Test.Shardagnostic.Network.TxSubmission (tests)
import qualified Shardagnostic.Network.Protocol.ChainSync.Test (tests)
import qualified Shardagnostic.Network.Protocol.BlockFetch.Test (tests)
import qualified Shardagnostic.Network.Protocol.Handshake.Test (tests)
import qualified Shardagnostic.Network.Protocol.TxSubmission.Test (tests)
import qualified Shardagnostic.Network.Protocol.TxSubmission2.Test (tests)
import qualified Shardagnostic.Network.Protocol.LocalStateQuery.Test (tests)
import qualified Shardagnostic.Network.Protocol.LocalTxSubmission.Test (tests)
import qualified Shardagnostic.Network.Protocol.KeepAlive.Test (tests)
import qualified Shardagnostic.Network.Protocol.TipSample.Test (tests)
import qualified Test.Shardagnostic.Network.PeerSelection (tests)
import qualified Test.Socket (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "shardagnostic-network"

    -- data structures
  [ Test.ChainGenerators.tests
  , Test.Chain.tests
  , Test.AnchoredFragment.tests
  , Test.ChainProducerState.tests

    -- protocols
  , Shardagnostic.Network.Protocol.ChainSync.Test.tests
  , Shardagnostic.Network.Protocol.BlockFetch.Test.tests
  , Shardagnostic.Network.Protocol.LocalStateQuery.Test.tests
  , Shardagnostic.Network.Protocol.LocalTxSubmission.Test.tests
  , Shardagnostic.Network.Protocol.TxSubmission.Test.tests
  , Shardagnostic.Network.Protocol.TxSubmission2.Test.tests
  , Shardagnostic.Network.Protocol.Handshake.Test.tests
  , Shardagnostic.Network.Protocol.KeepAlive.Test.tests
  , Shardagnostic.Network.Protocol.TipSample.Test.tests

    -- network logic
  , Test.Version.tests
  , Test.Pipe.tests
  , Test.Socket.tests
  , Test.PeerState.tests
  , Test.Shardagnostic.Network.BlockFetch.tests
  , Test.Shardagnostic.Network.PeerSelection.tests
  , Test.Shardagnostic.Network.KeepAlive.tests
  , Test.Shardagnostic.Network.TxSubmission.tests
  , Test.Shardagnostic.Network.NodeToNode.Version.tests
  , Test.Shardagnostic.Network.NodeToClient.Version.tests
  , Test.LedgerPeers.tests

    -- pseudo system-level
  , Test.Shardagnostic.Network.MockNode.tests
  ]
