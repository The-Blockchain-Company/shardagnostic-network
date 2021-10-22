
-- | Drivers for running 'Peer's with a 'Codec' and a 'Channel'.
--
module Shardagnostic.Network.Driver (

  -- * Normal peers
  runPeer,
  runPeerWithLimits,
  TraceSendRecv(..),

  -- * Pipelined peers
  runPipelinedPeer,
  runPipelinedPeerWithLimits,
  ) where

import Shardagnostic.Network.Driver.Simple
import Shardagnostic.Network.Driver.Limits

