{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Block.Sophie (
    Args (..)
  , SophieBlockArgs
  ) where

import qualified Data.Aeson as Aeson
import           Data.Foldable (asum, toList)
import qualified Data.Map.Strict as Map
import           Data.Sequence.Strict (StrictSeq)
import           GHC.Records (HasField, getField)
import           Options.Applicative

import qualified Bcc.Ledger.Core as Core
import qualified Bcc.Ledger.Era as CL
import qualified Sophie.Spec.Ledger.API as SL

import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits
import           Shardagnostic.Consensus.Node.ProtocolInfo

import           Shardagnostic.Consensus.Sophie.Eras (SophieBasedEra,
                     StandardSophie)
import           Shardagnostic.Consensus.Sophie.Ledger.Block (SophieBlock)
import qualified Shardagnostic.Consensus.Sophie.Ledger.Block as Sophie
import           Shardagnostic.Consensus.Sophie.Node (Nonce (..),
                     ProtocolParamsSophie (..),
                     ProtocolParamsSophieBased (..), SophieGenesis,
                     protocolInfoSophie)

import           HasAnalysis

-- | Usable for each Sophie-based era
instance ( SophieBasedEra era
         , HasField "outputs" (Core.TxBody era) (StrictSeq (Core.TxOut era))
         ) => HasAnalysis (SophieBlock era) where
  countTxOutputs blk = case Sophie.sophieBlockRaw blk of
      SL.Block _ body -> sum $ fmap countOutputs (CL.fromTxSeq @era body)
    where
      countOutputs :: Core.Tx era -> Int
      countOutputs = length . getField @"outputs" . getField @"body"

  blockTxSizes blk = case Sophie.sophieBlockRaw blk of
      SL.Block _ body ->
          toList
        $ fmap (fromIntegral . (getField @"txsize")) (CL.fromTxSeq @era body)

  knownEBBs = const Map.empty

-- | Sophie-era specific
instance HasProtocolInfo (SophieBlock StandardSophie) where
  data Args (SophieBlock StandardSophie) = SophieBlockArgs {
        configFileSophie :: FilePath
      , initialNonce      :: Nonce
      }
    deriving (Show)

  argsParser _ = parseSophieArgs
  mkProtocolInfo SophieBlockArgs {..}  = do
    config <- either (error . show) return =<<
      Aeson.eitherDecodeFileStrict' configFileSophie
    return $ mkSophieProtocolInfo config initialNonce

type SophieBlockArgs = Args (SophieBlock StandardSophie)

mkSophieProtocolInfo ::
     SophieGenesis StandardSophie
  -> Nonce
  -> ProtocolInfo IO (SophieBlock StandardSophie)
mkSophieProtocolInfo genesis initialNonce =
    protocolInfoSophie
      ProtocolParamsSophieBased {
          sophieBasedGenesis           = genesis
        , sophieBasedInitialNonce      = initialNonce
        , sophieBasedLeaderCredentials = []
        }
      ProtocolParamsSophie {
          sophieProtVer                = SL.ProtVer 2 0
        , sophieMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
        }

parseSophieArgs :: Parser SophieBlockArgs
parseSophieArgs = SophieBlockArgs
    <$> strOption (mconcat [
            long "configSophie"
          , help "Path to config file"
          , metavar "PATH"
          ])
    <*> asum [ Nonce  <$> parseNonce
             , pure NeutralNonce]
  where
    parseNonce = strOption (mconcat [
            long "nonce"
          , help "Initial nonce, i.e., hash of the genesis config file"
          , metavar "NONCE"
          ])
