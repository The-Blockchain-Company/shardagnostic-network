{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.ThreadNet.Infra.Cole.Genesis (
    colePBftParams
  , generateGenesisConfig
  ) where

import           Control.Monad.Except (runExceptT)

import           Shardagnostic.Consensus.BlockchainTime
import           Shardagnostic.Consensus.Config.SecurityParam
import           Shardagnostic.Consensus.Node.ProtocolInfo
import           Shardagnostic.Consensus.Protocol.PBFT

import qualified Bcc.Chain.Common as Common
import qualified Bcc.Chain.Genesis as Genesis
import qualified Bcc.Chain.Update as Update
import qualified Bcc.Crypto as Crypto
import qualified Test.Bcc.Chain.Genesis.Dummy as Dummy

import           Shardagnostic.Consensus.Cole.Ledger.Conversions

import           Test.Util.Time

{-------------------------------------------------------------------------------
  Generating the genesis configuration
-------------------------------------------------------------------------------}

colePBftParams :: SecurityParam -> NumCoreNodes -> PBftParams
colePBftParams paramK numCoreNodes = PBftParams
  { pbftNumNodes           = numCoreNodes
  , pbftSecurityParam      = paramK
  , pbftSignatureThreshold = PBftSignatureThreshold $ (1 / n) + (1 / k) + epsilon
    -- crucially: @floor (k * t) >= ceil (k / n)@
  }
    where
      epsilon = 1/10000   -- avoid problematic floating point round-off

      n :: Num a => a
      n = fromIntegral x where NumCoreNodes x = numCoreNodes

      k :: Num a => a
      k = fromIntegral x where SecurityParam x = paramK

-- Instead of using 'Dummy.dummyConfig', which hard codes the number of rich
-- men (= CoreNodes for us) to 4, we generate a dummy config with the given
-- number of rich men.
generateGenesisConfig :: SlotLength
                      -> PBftParams
                      -> (Genesis.Config, Genesis.GeneratedSecrets)
generateGenesisConfig slotLen params =
    either (error . show) id $
      Crypto.deterministic "this is fake entropy for testing" $
        runExceptT $
          Genesis.generateGenesisConfigWithEntropy dawnOfTime spec
  where
    PBftParams{pbftNumNodes, pbftSecurityParam} = params
    NumCoreNodes numCoreNodes = pbftNumNodes

    spec :: Genesis.GenesisSpec
    spec = Dummy.dummyGenesisSpec
        { Genesis.gsInitializer = Dummy.dummyGenesisInitializer
          { Genesis.giTestBalance =
              (Genesis.giTestBalance Dummy.dummyGenesisInitializer)
                -- The nodes are the richmen
                { Genesis.tboRichmen = fromIntegral numCoreNodes }
          }
        , Genesis.gsK = Common.BlockCount $ maxRollbacks pbftSecurityParam
        , Genesis.gsProtocolParameters = gsProtocolParameters
          { Update.ppSlotDuration = toColeSlotLength slotLen
          }
        }
      where
        gsProtocolParameters = Genesis.gsProtocolParameters Dummy.dummyGenesisSpec
