{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.Util.HasCreator.Mock () where

import           Bcc.Crypto.DSIGN
import           Data.Word (Word64)

import           Shardagnostic.Consensus.NodeId (CoreNodeId (..))

import           Shardagnostic.Consensus.Mock.Ledger
import           Shardagnostic.Consensus.Mock.Protocol.Optimum
import           Shardagnostic.Consensus.Protocol.BFT
import           Shardagnostic.Consensus.Protocol.PBFT

import           Test.ThreadNet.Util.HasCreator


instance HasCreator (SimpleBftBlock c BftMockCrypto) where
    getCreator = coreNodeId
               . bftSignature
               . simpleBftExt
               . simpleHeaderExt
               . simpleHeader
      where
        coreNodeId :: SignedDSIGN MockDSIGN a -> CoreNodeId
        coreNodeId = CoreNodeId . verKeyIdFromSigned

instance HasCreator (SimplePBftBlock c PBftMockCrypto) where
    getCreator = coreNodeId
               . pbftSignature
               . simplePBftExt
               . simpleHeaderExt
               . simpleHeader
      where
        coreNodeId :: SignedDSIGN MockDSIGN a -> CoreNodeId
        coreNodeId = CoreNodeId . verKeyIdFromSigned

instance HasCreator (SimpleOptimumBlock c OptimumMockCrypto) where
    getCreator = optimumCreator
               . optimumExtraFields
               . simpleOptimumExt
               . simpleHeaderExt
               . simpleHeader

instance HasCreator (SimpleOptimumRuleBlock c) where
    getCreator = simpleOptimumRuleExt
               . simpleHeaderExt
               . simpleHeader

-- | Get the id of the signer from a signature. Used for testing.
verKeyIdFromSigned :: SignedDSIGN MockDSIGN a -> Word64
verKeyIdFromSigned (SignedDSIGN (SigMockDSIGN _ i)) = i
