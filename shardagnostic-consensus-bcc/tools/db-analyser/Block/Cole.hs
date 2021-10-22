{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Block.Cole (
    Args (..)
  , ColeBlockArgs
  , openGenesisCole
  ) where

import           Control.Monad.Except
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (asum)
import           Options.Applicative

import           Bcc.Binary (Raw, unAnnotated)
import           Bcc.Crypto (RequiresNetworkMagic (..))
import qualified Bcc.Crypto as Crypto

import qualified Bcc.Chain.Block as Chain
import qualified Bcc.Chain.Genesis as Genesis
import qualified Bcc.Chain.UTxO as Chain
import qualified Bcc.Chain.Update as Update

import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits
import           Shardagnostic.Consensus.Node.ProtocolInfo

import           Shardagnostic.Consensus.Cole.Ledger (ColeBlock)
import qualified Shardagnostic.Consensus.Cole.Ledger as Cole
import           Shardagnostic.Consensus.Cole.Node (PBftSignatureThreshold (..),
                     ProtocolParamsCole (..), protocolInfoCole)

import           HasAnalysis

instance HasAnalysis ColeBlock where
    countTxOutputs = aBlockOrBoundary (const 0) countTxOutputsCole
    blockTxSizes = aBlockOrBoundary (const []) blockTxSizesCole
    knownEBBs = const Cole.knownEBBs

instance HasProtocolInfo ColeBlock where
    data Args ColeBlock =
      ColeBlockArgs {
          configFileCole      :: FilePath
        , requiresNetworkMagic :: RequiresNetworkMagic
        , genesisHash          :: Maybe (Crypto.Hash Raw)
        , threshold            :: Maybe PBftSignatureThreshold
        }
    argsParser _ = parseColeArgs
    mkProtocolInfo ColeBlockArgs {..} = do
      config <- openGenesisCole configFileCole genesisHash requiresNetworkMagic
      return $ mkColeProtocolInfo config threshold

type ColeBlockArgs = Args ColeBlock

parseColeArgs :: Parser ColeBlockArgs
parseColeArgs = ColeBlockArgs
    <$> strOption (mconcat [
            long "configCole"
          , help "Path to config file"
          , metavar "PATH"
          ])
    <*> flag RequiresNoMagic RequiresMagic (mconcat [
            long "requires-magic"
          , help "The DB contains blocks from a testnet, requiring network magic, rather than mainnet"
          ])
    <*> parseMaybe (option auto (mconcat [
            long "genesisHash"
          , help "Expected genesis hash"
          , metavar "HASH"
          ]))
    <*> parseMaybe (PBftSignatureThreshold <$> thresholdParser)
  where
    thresholdParser = option auto (mconcat [
            long "threshold"
          , help "PBftSignatureThreshold"
          , metavar "THRESHOLD"
          ])

parseMaybe ::  Parser a -> Parser (Maybe a)
parseMaybe parser = asum [Just <$> parser, pure Nothing]

-- | Equivalent of 'either' for 'ABlockOrBoundary'.
aBlockOrBoundary :: (Chain.ABoundaryBlock ByteString -> a)
                 -> (Chain.ABlock ByteString -> a)
                 -> ColeBlock -> a
aBlockOrBoundary fromBoundary fromRegular blk = case blk of
    Cole.ColeBlock (Chain.ABOBBoundary boundaryBlock) _ _
      -> fromBoundary boundaryBlock
    Cole.ColeBlock (Chain.ABOBBlock regularBlk) _ _
      -> fromRegular regularBlk

countTxOutputsCole :: Chain.ABlock ByteString -> Int
countTxOutputsCole Chain.ABlock{..} = countTxPayload bodyTxPayload
  where
    Chain.ABody { bodyTxPayload } = blockBody

    countTxPayload :: Chain.ATxPayload a -> Int
    countTxPayload = sum
                   . map (countTx . unAnnotated . Chain.aTaTx)
                   . Chain.aUnTxPayload

    countTx :: Chain.Tx -> Int
    countTx = length . Chain.txOutputs

blockTxSizesCole :: Chain.ABlock ByteString -> [SizeInBytes]
blockTxSizesCole block =
    map (fromIntegral . BL.length . BL.fromStrict . Chain.aTaAnnotation) blockTxAuxs
  where
    Chain.ABlock{ blockBody } = block
    Chain.ABody{ bodyTxPayload } = blockBody
    Chain.ATxPayload{ aUnTxPayload = blockTxAuxs } = bodyTxPayload

openGenesisCole ::
     FilePath
  -> Maybe (Crypto.Hash Raw)
  -> RequiresNetworkMagic
  -> IO Genesis.Config
openGenesisCole configFile mHash requiresNetworkMagic = do
    genesisHash <- case mHash of
      Nothing -> either (error . show) return =<< runExceptT
        (Genesis.unGenesisHash . snd <$> Genesis.readGenesisData configFile)
      Just hash -> return hash
    genesisConfig <- either (error . show) return =<< runExceptT
      (Genesis.mkConfigFromFile
        requiresNetworkMagic
        configFile
        genesisHash)
    return genesisConfig

mkColeProtocolInfo :: Genesis.Config
                    -> Maybe PBftSignatureThreshold
                    -> ProtocolInfo IO ColeBlock
mkColeProtocolInfo genesisConfig signatureThreshold =
    protocolInfoCole $ ProtocolParamsCole {
        coleGenesis                = genesisConfig
      , colePbftSignatureThreshold = signatureThreshold
      , coleProtocolVersion        = Update.ProtocolVersion 1 0 0
      , coleSoftwareVersion        = Update.SoftwareVersion (Update.ApplicationName "db-analyser") 2
      , coleLeaderCredentials      = Nothing
      , coleMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
      }
