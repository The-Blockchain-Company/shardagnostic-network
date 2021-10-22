{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.ThreadNet.Infra.Cole.ProtocolInfo (
    mkLeaderCredentials
  , mkProtocolCole
  , theProposedProtocolVersion
  , theProposedSoftwareVersion
  ) where

import           Data.Foldable (find)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           GHC.Stack (HasCallStack)

import qualified Bcc.Chain.Common as Common
import qualified Bcc.Chain.Delegation as Delegation
import qualified Bcc.Chain.Genesis as Genesis
import qualified Bcc.Chain.Update as Update
import qualified Bcc.Crypto as Crypto

import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits
import           Shardagnostic.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import           Shardagnostic.Consensus.NodeId (CoreNodeId (..))
import           Shardagnostic.Consensus.Protocol.PBFT

import           Shardagnostic.Consensus.Cole.Crypto.DSIGN (ColeDSIGN,
                     SignKeyDSIGN (..))
import           Shardagnostic.Consensus.Cole.Ledger (ColeBlock)
import           Shardagnostic.Consensus.Cole.Node

mkProtocolCole ::
     forall m. (Monad m, HasCallStack)
  => PBftParams
  -> CoreNodeId
  -> Genesis.Config
  -> Genesis.GeneratedSecrets
  -> (ProtocolInfo m ColeBlock, SignKeyDSIGN ColeDSIGN)
     -- ^ We return the signing key which is needed in some tests, because it
     -- cannot easily be extracted from the 'ProtocolInfo'.
mkProtocolCole params coreNodeId genesisConfig genesisSecrets =
    (protocolInfo, signingKey)
  where
    leaderCredentials :: ColeLeaderCredentials
    leaderCredentials =
        mkLeaderCredentials
          genesisConfig
          genesisSecrets
          coreNodeId

    signingKey :: SignKeyDSIGN ColeDSIGN
    signingKey = SignKeyColeDSIGN (blcSignKey leaderCredentials)

    PBftParams { pbftSignatureThreshold } = params

    protocolInfo :: ProtocolInfo m ColeBlock
    protocolInfo =
        protocolInfoCole $ ProtocolParamsCole {
            coleGenesis                = genesisConfig
          , colePbftSignatureThreshold = Just $ pbftSignatureThreshold
          , coleProtocolVersion        = theProposedProtocolVersion
          , coleSoftwareVersion        = theProposedSoftwareVersion
          , coleLeaderCredentials      = Just leaderCredentials
          , coleMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
          }

mkLeaderCredentials
  :: HasCallStack
  => Genesis.Config
  -> Genesis.GeneratedSecrets
  -> CoreNodeId
  -> ColeLeaderCredentials
mkLeaderCredentials genesisConfig genesisSecrets (CoreNodeId i) =
    either (error . show) id $
      mkColeLeaderCredentials
        genesisConfig
        dlgKey
        dlgCert
        "ThreadNet"
  where
    dlgKey :: Crypto.SigningKey
    dlgKey = fromMaybe (error "dlgKey") $
       find (\sec -> Delegation.delegateVK dlgCert == Crypto.toVerification sec)
            $ Genesis.gsRichSecrets genesisSecrets

    dlgCert :: Delegation.Certificate
    dlgCert = snd $ Map.toAscList dlgMap !! (fromIntegral i)

    dlgMap :: Map Common.KeyHash Delegation.Certificate
    dlgMap = Genesis.unGenesisDelegation
           $ Genesis.gdHeavyDelegation
           $ Genesis.configGenesisData genesisConfig

-- | The protocol version proposed as part of the hard-fork smoke test
--
-- The initial Cole ledger state begins with protocol version @0.0.0@. In the
-- smoke test, if the proposal and votes are enabled, then we will be proposing
-- an update to @1.0.0@.
--
-- This value occurs in two crucial places: the proposal and also the
-- 'Cole.coleProtocolVersion' field of the static node config. See the
-- Haddock comment on 'mkProtocolColeAndHardForkTxs'.
--
theProposedProtocolVersion :: Update.ProtocolVersion
theProposedProtocolVersion = Update.ProtocolVersion 1 0 0

-- | The software version proposed as part of the hard-fork smoke test
--
-- We don't actually care about this for the smoke test, but we have to set it
-- both as part of the proposal and also as part of the node's static
-- configuration. Its use in the static configuration is legacy and does not
-- seem to affect anything; see Issue #1732.
--
-- The initial Cole ledger state begins with no recorded software versions.
-- For the addition of a new software version, the Cole ledger rules require
-- that it starts at 0 or 1.
--
theProposedSoftwareVersion :: Update.SoftwareVersion
theProposedSoftwareVersion = Update.SoftwareVersion
  -- appnames must be ASCII and <= 12 characters
  (Update.ApplicationName "Dummy")
  0
