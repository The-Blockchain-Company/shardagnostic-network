{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Shardagnostic.Consensus.Sophie.Ledger.NetworkProtocolVersion (
    SophieNodeToClientVersion (..)
  , SophieNodeToNodeVersion (..)
  ) where

import qualified Data.Map.Strict as Map

import           Shardagnostic.Consensus.Node.NetworkProtocolVersion

import           Shardagnostic.Consensus.Sophie.Ledger.Block

data SophieNodeToNodeVersion = SophieNodeToNodeVersion1
  deriving (Show, Eq, Ord, Enum, Bounded)

data SophieNodeToClientVersion =
    SophieNodeToClientVersion1

    -- | New queries introduced
  | SophieNodeToClientVersion2

    -- | New query introduced
  | SophieNodeToClientVersion3

    -- | New queries introduced
  | SophieNodeToClientVersion4
  deriving (Show, Eq, Ord, Enum, Bounded)

instance HasNetworkProtocolVersion (SophieBlock era) where
  type BlockNodeToNodeVersion   (SophieBlock era) = SophieNodeToNodeVersion
  type BlockNodeToClientVersion (SophieBlock era) = SophieNodeToClientVersion

-- TODO #2668 make this era-specific
instance SupportedNetworkProtocolVersion (SophieBlock era) where
  supportedNodeToNodeVersions   _ = Map.fromList [
        (NodeToNodeV_1, SophieNodeToNodeVersion1)
        -- V_2 enables block size hints for Cole headers and the hard fork
        -- combinator, unused by Sophie-only
      ]
  supportedNodeToClientVersions _ = Map.fromList [
        (NodeToClientV_1, SophieNodeToClientVersion1)
        -- Enable the LocalStateQuery protocol, no serialisation changes
      , (NodeToClientV_2, SophieNodeToClientVersion1)
        -- V_3 enables the hard fork to Sophie, which didn't affect
        -- Sophie-only when introduced. However, we have retroactively claimed
        -- V_3 to enable 'SophieNodeToClientVersion2'.
      , (NodeToClientV_3, SophieNodeToClientVersion2)
        -- V_4 enables the hard fork to Evie, which didn't affect
        -- Sophie-only when introduced. However, we have retroactively claimed
        -- V_4 to enable 'SophieNodeToClientVersion3'.
      , (NodeToClientV_4, SophieNodeToClientVersion3)
      ]

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault
