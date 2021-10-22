{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Block.Aurum (
    AurumBlockArgs
  , Args (..)
  ) where

import           Control.Applicative
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Options.Applicative
import           Prelude

import           Data.Aeson (FromJSON (..), (.:), (.:?))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (FromJSONKey (..))

import qualified Bcc.Ledger.Aurum.Genesis as Aurum
import qualified Bcc.Ledger.Aurum.Language as Aurum
import qualified Bcc.Ledger.Aurum.Scripts as Aurum
import qualified Bcc.Ledger.BaseTypes as Ledger
import           HasAnalysis (HasProtocolInfo (..))
import           Shardagnostic.Consensus.Sophie.Eras (StandardAurum)
import           Shardagnostic.Consensus.Sophie.Ledger.Block (SophieBlock)

import           Zerepoch.V1.Ledger.Api (defaultCostModelParams)

instance HasProtocolInfo (SophieBlock StandardAurum) where
  data Args (SophieBlock StandardAurum) = AurumBlockArgs {
        configFileAurum :: FilePath
      }
    deriving (Show)

  argsParser _ = AurumBlockArgs
    <$> strOption (mconcat [
            long "configAurum"
          , help "Path to config file"
          , metavar "PATH"
          ])

  -- | This function would only be used if we run an
  -- Aurum only chain. This should be dead code really.
  mkProtocolInfo _ =
    error $ "Not implemented because we don't "
         <> "anticipate running an 'Aurum only' chain."

type AurumBlockArgs = Args (SophieBlock StandardAurum)

instance FromJSON Aurum.AurumGenesis where
  parseJSON = Aeson.withObject "Aurum Genesis" $ \o -> do
    coinsPerUTxOWord     <- o .:  "entropicPerUTxOWord"
                        <|> o .:  "bccPerUTxOWord"
    cModels              <- o .:? "costModels"
    prices               <- o .:  "executionPrices"
    maxTxExUnits         <- o .:  "maxTxExUnits"
    maxBlockExUnits      <- o .:  "maxBlockExUnits"
    maxValSize           <- o .:  "maxValueSize"
    collateralPercentage <- o .:  "collateralPercentage"
    maxCollateralInputs  <- o .:  "maxCollateralInputs"
    case cModels of
      Nothing -> case Aurum.CostModel <$> defaultCostModelParams of
        Just m -> return Aurum.AurumGenesis
          { Aurum.coinsPerUTxOWord
          , Aurum.costmdls = Map.singleton Aurum.ZerepochV1 m
          , Aurum.prices
          , Aurum.maxTxExUnits
          , Aurum.maxBlockExUnits
          , Aurum.maxValSize
          , Aurum.collateralPercentage
          , Aurum.maxCollateralInputs
          }
        Nothing -> fail "Failed to extract the cost model params from defaultCostModel"
      Just costmdls -> return Aurum.AurumGenesis
        { Aurum.coinsPerUTxOWord
        , Aurum.costmdls
        , Aurum.prices
        , Aurum.maxTxExUnits
        , Aurum.maxBlockExUnits
        , Aurum.maxValSize
        , Aurum.collateralPercentage
        , Aurum.maxCollateralInputs
        }

deriving instance FromJSON Aurum.ExUnits

instance FromJSON Aurum.Language where
  parseJSON = Aeson.withText "Language" languageFromText

instance FromJSONKey Aurum.Language where
  fromJSONKey = Aeson.FromJSONKeyTextParser languageFromText

instance FromJSON Aurum.Prices where
  parseJSON =
    Aeson.withObject "prices" $ \o -> do
      steps <- o .: "prSteps"
      mem   <- o .: "prMem"
      prSteps <- checkBoundedRational steps
      prMem   <- checkBoundedRational mem
      return Aurum.Prices { Aurum.prSteps, Aurum.prMem }
    where
      -- We cannot round-trip via NonNegativeInterval, so we go via Rational
      checkBoundedRational r =
        case Ledger.boundRational r of
          Nothing -> fail ("too much precision for bounded rational: " ++ show r)
          Just s  -> return s

deriving newtype instance FromJSON Aurum.CostModel

languageFromText :: MonadFail m => Text -> m Aurum.Language
languageFromText "ZerepochV1" = pure Aurum.ZerepochV1
languageFromText lang       = fail $ "Error decoding Language: " ++ show lang
