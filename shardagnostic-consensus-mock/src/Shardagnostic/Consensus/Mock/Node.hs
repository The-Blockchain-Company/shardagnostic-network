{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Shardagnostic.Consensus.Mock.Node (
    CodecConfig (..)
  , simpleBlockForging
  ) where

import           Codec.Serialise (Serialise)
import qualified Data.Map.Strict as Map
import           Data.Void (Void)

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Config
import           Shardagnostic.Consensus.Ledger.SupportsMempool (txForgetValidated)
import           Shardagnostic.Consensus.Ledger.SupportsProtocol
import           Shardagnostic.Consensus.Mock.Ledger
import           Shardagnostic.Consensus.Mock.Node.Abstract
import           Shardagnostic.Consensus.Mock.Node.Serialisation ()
import           Shardagnostic.Consensus.Node.InitStorage
import           Shardagnostic.Consensus.Node.NetworkProtocolVersion
import           Shardagnostic.Consensus.Node.Run
import           Shardagnostic.Consensus.Protocol.Abstract
import           Shardagnostic.Consensus.Util.RedundantConstraints

import           Shardagnostic.Consensus.Storage.ImmutableDB (simpleChunkInfo)

{-------------------------------------------------------------------------------
  RunNode instance for the mock ledger
-------------------------------------------------------------------------------}

instance HasNetworkProtocolVersion (SimpleBlock SimpleMockCrypto ext) where
  -- Use defaults

instance SupportedNetworkProtocolVersion (SimpleBlock SimpleMockCrypto ext) where
  supportedNodeToNodeVersions   _ = Map.singleton maxBound ()
  supportedNodeToClientVersions _ = Map.singleton maxBound ()

  latestReleasedNodeVersion = latestReleasedNodeVersionDefault

instance NodeInitStorage (SimpleBlock SimpleMockCrypto ext) where
  nodeImmutableDbChunkInfo (SimpleStorageConfig secParam) = simpleChunkInfo $
      EpochSize $ 10 * maxRollbacks secParam

  nodeCheckIntegrity _ _ = True

instance BlockSupportsMetrics (SimpleBlock c ext) where
  isSelfIssued = isSelfIssuedConstUnknown

instance ( LedgerSupportsProtocol      (SimpleBlock SimpleMockCrypto ext)
         , Show (CannotForge           (SimpleBlock SimpleMockCrypto ext))
         , Show (ForgeStateInfo        (SimpleBlock SimpleMockCrypto ext))
         , Show (ForgeStateUpdateError (SimpleBlock SimpleMockCrypto ext))
         , Serialise ext
         , RunMockBlock SimpleMockCrypto ext
         ) => RunNode (SimpleBlock SimpleMockCrypto ext)

{-------------------------------------------------------------------------------
  BlockForging
-------------------------------------------------------------------------------}

-- | Can be used when 'CanBeLeader' is static
simpleBlockForging ::
     forall c ext m.
     ( RunMockBlock c ext
     , CannotForge           (SimpleBlock c ext) ~ Void
     , ForgeStateInfo        (SimpleBlock c ext) ~ ()
     , ForgeStateUpdateError (SimpleBlock c ext) ~ Void
     , Monad m
     )
  => CanBeLeader (BlockProtocol (SimpleBlock c ext))
  -> ForgeExt c ext
  -> BlockForging m (SimpleBlock c ext)
simpleBlockForging canBeLeader forgeExt = BlockForging {
      forgeLabel       = "simpleBlockForging"
    , canBeLeader      = canBeLeader
    , updateForgeState = \_ _ _ -> return $ ForgeStateUpdated ()
    , checkCanForge    = \_ _ _ _ _ -> return ()
    , forgeBlock       = \cfg bno slot lst txs proof ->
        return
          $ forgeSimple
              forgeExt
              cfg
              bno
              slot
              lst
              (map txForgetValidated txs)
              proof
    }
  where
    _ = keepRedundantConstraint (Proxy @(ForgeStateUpdateError (SimpleBlock c ext) ~ Void))
