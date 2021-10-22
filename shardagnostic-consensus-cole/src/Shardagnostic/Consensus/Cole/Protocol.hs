{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Shardagnostic.Consensus.Cole.Protocol (
    PBftColeCrypto
  , genesisKeyCoreNodeId
  , nodeIdToGenesisKey
  ) where

import           Control.Monad (guard)
import           Data.Set (Set)
import qualified Data.Set as Set

import qualified Bcc.Chain.Common as CC.Common
import qualified Bcc.Chain.Delegation as CC.Delegation
import qualified Bcc.Chain.Genesis as CC.Genesis

import           Shardagnostic.Consensus.NodeId (CoreNodeId (..))
import           Shardagnostic.Consensus.Protocol.PBFT

import           Shardagnostic.Consensus.Cole.Crypto.DSIGN

{-------------------------------------------------------------------------------
  Crypto
-------------------------------------------------------------------------------}

data PBftColeCrypto

instance PBftCrypto PBftColeCrypto where
  type PBftDSIGN          PBftColeCrypto = ColeDSIGN
  type PBftDelegationCert PBftColeCrypto = CC.Delegation.Certificate
  type PBftVerKeyHash     PBftColeCrypto = CC.Common.KeyHash

  dlgCertGenVerKey = VerKeyColeDSIGN . CC.Delegation.issuerVK
  dlgCertDlgVerKey = VerKeyColeDSIGN . CC.Delegation.delegateVK
  hashVerKey (VerKeyColeDSIGN pk) = CC.Common.hashKey pk

{-------------------------------------------------------------------------------
  PBFT node order
-------------------------------------------------------------------------------}

-- | Determine the 'CoreNodeId' for a code node, based on the genesis key it
-- will sign blocks on behalf of.
--
-- In PBFT, the 'CoreNodeId' index is determined by the 0-based position in
-- the sort order of the genesis key hashes.
genesisKeyCoreNodeId :: CC.Genesis.Config
                     -> VerKeyDSIGN ColeDSIGN
                        -- ^ The genesis verification key
                     -> Maybe CoreNodeId
genesisKeyCoreNodeId gc vkey =
    CoreNodeId . fromIntegral <$>
      Set.lookupIndex (hashVerKey vkey) (genesisKeyHashes gc)

-- | Inverse of 'genesisKeyCoreNodeId'
nodeIdToGenesisKey :: CC.Genesis.Config
                   -> CoreNodeId
                   -> Maybe CC.Common.KeyHash
nodeIdToGenesisKey gc (CoreNodeId nid) = do
    guard $ nid < fromIntegral (Set.size (genesisKeyHashes gc))
    return $ Set.elemAt (fromIntegral nid) (genesisKeyHashes gc)

genesisKeyHashes :: CC.Genesis.Config -> Set CC.Common.KeyHash
genesisKeyHashes = CC.Genesis.unGenesisKeyHashes
                 . CC.Genesis.configGenesisKeyHashes
