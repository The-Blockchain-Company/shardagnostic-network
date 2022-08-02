{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Block.Bcc (
    Args (..)
  , BccBlockArgs
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import           Data.SOP.Strict
import           Options.Applicative

import qualified Bcc.Chain.Genesis as Cole.Genesis
import qualified Bcc.Chain.Update as Cole.Update

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.HardFork.Combinator (HardForkBlock (..),
                     OneEraBlock (..), OneEraHash (..))
import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits
import           Shardagnostic.Consensus.Node.ProtocolInfo

import qualified Bcc.Ledger.Aurum.Genesis as SL (AurumGenesis)
import           Bcc.Ledger.Crypto

import           Shardagnostic.Consensus.Cole.Ledger (ColeBlock)

import           Shardagnostic.Consensus.Bcc
import           Shardagnostic.Consensus.Bcc.Node (TriggerHardFork (..),
                     protocolInfoBcc)
import           Shardagnostic.Consensus.Sophie.Eras (StandardAurum,
                     StandardSophie)
import           Shardagnostic.Consensus.Sophie.Ledger.Block (SophieBlock)

import           Block.Aurum (Args (..))
import           Block.Cole (Args (..), openGenesisCole)
import           Block.Sophie (Args (..))
import           HasAnalysis

analyseBlock ::
     (forall blk. HasAnalysis blk => blk -> a)
  -> BccBlock StandardCrypto -> a
analyseBlock f =
      hcollapse
    . hcmap p (K . f . unI)
    . getOneEraBlock
    . getHardForkBlock
  where
    p :: Proxy HasAnalysis
    p = Proxy

instance HasProtocolInfo (BccBlock StandardCrypto) where
  data Args (BccBlock StandardCrypto) =
    BccBlockArgs {
        coleArgs    :: Args ColeBlock
      , sophieArgs      :: Args (SophieBlock StandardSophie)
      , aurumArgs      :: Args (SophieBlock StandardAurum)
      }
  argsParser _ = parseBccArgs
  mkProtocolInfo BccBlockArgs {..} = do
    let ColeBlockArgs {..}   = coleArgs
    let SophieBlockArgs {..} = sophieArgs
    let AurumBlockArgs {..} = aurumArgs
    genesisCole <- openGenesisCole configFileCole genesisHash requiresNetworkMagic
    genesisSophie <- either (error . show) return =<<
      Aeson.eitherDecodeFileStrict' configFileSophie
    genesisAurum <- either (error . show) return =<<
      Aeson.eitherDecodeFileStrict' configFileAurum
    return $ mkBccProtocolInfo genesisCole threshold genesisSophie genesisAurum initialNonce

instance HasAnalysis (BccBlock StandardCrypto) where
  countTxOutputs = analyseBlock countTxOutputs
  blockTxSizes   = analyseBlock blockTxSizes
  knownEBBs _    =
      Map.mapKeys castHeaderHash . Map.map castChainHash $
        knownEBBs (Proxy @ColeBlock)

type BccBlockArgs = Args (BccBlock StandardCrypto)

parseBccArgs :: Parser BccBlockArgs
parseBccArgs = BccBlockArgs
    <$> argsParser Proxy
    <*> argsParser Proxy
    <*> argsParser Proxy

mkBccProtocolInfo ::
     Cole.Genesis.Config
  -> Maybe PBftSignatureThreshold
  -> SophieGenesis StandardSophie
  -> SL.AurumGenesis
  -> Nonce
  -> ProtocolInfo IO (BccBlock StandardCrypto)
mkBccProtocolInfo genesisCole signatureThreshold genesisSophie genesisAurum initialNonce =
    protocolInfoBcc
      ProtocolParamsCole {
          coleGenesis                = genesisCole
        , colePbftSignatureThreshold = signatureThreshold
        , coleProtocolVersion        = Cole.Update.ProtocolVersion 1 2
        , coleSoftwareVersion        = Cole.Update.SoftwareVersion (Cole.Update.ApplicationName "db-analyser") 2
        , coleLeaderCredentials      = Nothing
        , coleMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
      ProtocolParamsSophieBased {
          sophieBasedGenesis           = genesisSophie
        , sophieBasedInitialNonce      = initialNonce
        , sophieBasedLeaderCredentials = []
        }
      ProtocolParamsSophie {
          sophieProtVer                = ProtVer 3 0
        , sophieMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
      ProtocolParamsEvie {
          evieProtVer                = ProtVer 4 0
        , evieMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
      ProtocolParamsJen {
          jenProtVer                   = ProtVer 5 0
        , jenMaxTxCapacityOverrides    = TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
      ProtocolParamsAurum {
          aurumProtVer                 = ProtVer 5 0
        , aurumMaxTxCapacityOverrides  = TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }
      ProtocolTransitionParamsSophieBased {
          transitionTranslationContext = ()
        , transitionTrigger            = TriggerHardForkAtVersion 2
        }
      ProtocolTransitionParamsSophieBased {
          transitionTranslationContext = ()
        , transitionTrigger            = TriggerHardForkAtVersion 3
        }
      ProtocolTransitionParamsSophieBased {
          transitionTranslationContext = ()
        , transitionTrigger            = TriggerHardForkAtVersion 4
        }
      ProtocolTransitionParamsSophieBased {
          transitionTranslationContext = genesisAurum
        , transitionTrigger            = TriggerHardForkAtVersion 5
        }

castHeaderHash ::
     HeaderHash ColeBlock
  -> HeaderHash (BccBlock StandardCrypto)
castHeaderHash = OneEraHash . toShortRawHash (Proxy @ColeBlock)

castChainHash ::
     ChainHash ColeBlock
  -> ChainHash (BccBlock StandardCrypto)
castChainHash GenesisHash   = GenesisHash
castChainHash (BlockHash h) = BlockHash $ castHeaderHash h
