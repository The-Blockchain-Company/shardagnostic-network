module Test.ThreadNet.Infra.Aurum (degenerateAurumGenesis) where

import qualified Data.Map as Map

import           Bcc.Ledger.Aurum.Genesis (AurumGenesis (..))
import           Bcc.Ledger.Aurum.Scripts (Prices (..))
import           Sophie.Spec.Ledger.API (Coin (..))

degenerateAurumGenesis :: AurumGenesis
degenerateAurumGenesis = AurumGenesis {
     coinsPerUTxOWord     = Coin 0
   , collateralPercentage = 0
   , costmdls             = Map.empty
   , maxBlockExUnits      = mempty
   , maxCollateralInputs  = 0
   , maxTxExUnits         = mempty
   , maxValSize           = 0
   , prices               = Prices minBound minBound
   }
