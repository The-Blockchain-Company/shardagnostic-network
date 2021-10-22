{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Shardagnostic.Consensus.Cole.Ledger.NetworkProtocolVersion (
    ColeNodeToClientVersion (..)
  , ColeNodeToNodeVersion (..)
  ) where

import qualified Data.Map.Strict as Map

import           Shardagnostic.Consensus.Node.NetworkProtocolVersion

import           Shardagnostic.Consensus.Cole.Ledger.Block

data ColeNodeToNodeVersion =
    -- | We send headers without a size hint
    ColeNodeToNodeVersion1

    -- | We send headers /with/ a size hint
  | ColeNodeToNodeVersion2
  deriving (Show, Eq, Ord, Enum, Bounded)

data ColeNodeToClientVersion =
    ColeNodeToClientVersion1
  deriving (Show, Eq, Ord, Enum, Bounded)

instance HasNetworkProtocolVersion ColeBlock where
  type BlockNodeToNodeVersion   ColeBlock = ColeNodeToNodeVersion
  type BlockNodeToClientVersion ColeBlock = ColeNodeToClientVersion

instance SupportedNetworkProtocolVersion ColeBlock where
  supportedNodeToNodeVersions   _ = Map.fromList [
        (NodeToNodeV_1, ColeNodeToNodeVersion1)
        -- V_2 enables block size hints for Cole headers within the hard fork
        -- combinator, not supported by Cole-only.
      ]
  supportedNodeToClientVersions _ = Map.fromList [
        (NodeToClientV_1, ColeNodeToClientVersion1)
        -- Enable the LocalStateQuery protocol, no serialisation changes
      , (NodeToClientV_2, ColeNodeToClientVersion1)
        -- V_3 enables the hard fork, not supported by Cole-only.
      ]

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault
