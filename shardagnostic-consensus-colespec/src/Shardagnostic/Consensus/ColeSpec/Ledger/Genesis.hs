{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE RecordWildCards    #-}

-- | Genesis config for the spec
--
-- Intended for qualified import
--
-- > import           Shardagnostic.Consensus.ColeSpec.Ledger.Genesis (ColeSpecGenesis)
-- > import qualified Shardagnostic.Consensus.ColeSpec.Ledger.Genesis as Genesis
module Shardagnostic.Consensus.ColeSpec.Ledger.Genesis (
    ColeSpecGenesis (..)
  , modFeeParams
  , modPBftThreshold
  , modPParams
  , modUtxo
  , modUtxoValues
    -- * Conversions
  , fromChainEnv
  , toChainEnv
  ) where

import           Data.Coerce (coerce)
import           Data.Set (Set)
import           NoThunks.Class (AllowThunk (..), NoThunks)
import           Numeric.Natural (Natural)

import qualified Cole.Spec.Chain.STS.Rule.Chain as Spec
import qualified Cole.Spec.Ledger.Core as Spec
import qualified Cole.Spec.Ledger.UTxO as Spec
import qualified Cole.Spec.Ledger.Update as Spec
import qualified Control.State.Transition as Spec

import           Shardagnostic.Consensus.ColeSpec.Ledger.Orphans ()

{-------------------------------------------------------------------------------
  Genesis config
-------------------------------------------------------------------------------}

-- | The equivalent of the genesis config for the abstract ledger
data ColeSpecGenesis = ColeSpecGenesis {
      coleSpecGenesisDelegators    :: Set Spec.VKeyGenesis
    , coleSpecGenesisInitUtxo      :: Spec.UTxO
    , coleSpecGenesisInitPParams   :: Spec.PParams
    , coleSpecGenesisSecurityParam :: Spec.BlockCount

      -- | Slot length
      --
      -- The Cole spec itself does not talk about slot length at all. Here we
      -- record it primarily to support the relation between the spec and the
      -- real implementation. For this reason we choose the same representation
      -- as the real PBFT does ('ppSlotDuration' in 'ProtocolParameters').
    , coleSpecGenesisSlotLength    :: Natural
    }
  deriving stock (Show)
  deriving NoThunks via AllowThunk ColeSpecGenesis

modPBftThreshold :: (Double -> Double)
                 -> ColeSpecGenesis -> ColeSpecGenesis
modPBftThreshold = modPParams . modPParamsPBftThreshold

-- | Modify the @a@ and @b@ fee parameters
modFeeParams :: ((Int, Int) -> (Int, Int))
             -> ColeSpecGenesis -> ColeSpecGenesis
modFeeParams = modPParams . modPParamsFeeParams

-- | Adjust all values in the initial UTxO equally
modUtxoValues :: (Integer -> Integer) -> ColeSpecGenesis -> ColeSpecGenesis
modUtxoValues = modUtxo . Spec.mapUTxOValues . coerce

modUtxo :: (Spec.UTxO -> Spec.UTxO) -> ColeSpecGenesis -> ColeSpecGenesis
modUtxo f genesis = genesis {
      coleSpecGenesisInitUtxo = f (coleSpecGenesisInitUtxo genesis)
    }

modPParams :: (Spec.PParams -> Spec.PParams)
           -> ColeSpecGenesis -> ColeSpecGenesis
modPParams f genesis = genesis {
      coleSpecGenesisInitPParams = f (coleSpecGenesisInitPParams genesis)
    }

{-------------------------------------------------------------------------------
  Internal: accessors for the protocol parameters
-------------------------------------------------------------------------------}

modPParamsPBftThreshold :: (Double -> Double)
                        -> Spec.PParams -> Spec.PParams
modPParamsPBftThreshold f pparams = pparams {
      Spec._bkSgnCntT = Spec.BkSgnCntT (f threshold)
    }
  where
    Spec.BkSgnCntT threshold = Spec._bkSgnCntT pparams

modPParamsFeeParams :: ((Int, Int) -> (Int, Int))
                    -> Spec.PParams -> Spec.PParams
modPParamsFeeParams f pparams = pparams {
      Spec._factorA = Spec.FactorA $ fst (f (a, b))
    , Spec._factorB = Spec.FactorB $ snd (f (a, b))
    }
  where
    Spec.FactorA a = Spec._factorA pparams
    Spec.FactorB b = Spec._factorB pparams

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

-- | Derive CHAIN rule environment
toChainEnv :: ColeSpecGenesis -> Spec.Environment Spec.CHAIN
toChainEnv ColeSpecGenesis{..} = disableConsensusChecks (
      Spec.Slot 0 -- current slot
    , coleSpecGenesisInitUtxo
    , coleSpecGenesisDelegators
    , coleSpecGenesisInitPParams
    , coleSpecGenesisSecurityParam
    )
  where
    -- We are only interested in updating the /ledger state/, not the /consensus
    -- chain state/. Unfortunately, the Cole spec does not make that
    -- distinction, and so when we call the CHAIN rule, we might get some errors
    -- here that the implementation does not report (because it would only find
    -- them when we update the chain state). There are at least two possible
    -- proper solutions for this:
    --
    -- 1. Modify the spec so that we /do/ have the separation. Note that if we
    --    did, we would not use the chain state part of the spec, since the
    --    chain state part of the dual ledger is determined entirely by the
    --    concrete Cole block.
    -- 2. Turn 'applyExtLedger' and related types into a type class of their
    --    own, so that we can override it specifically for the dual ledger.
    --
    -- Either way, we are only testing the /ledger/ part of the two blocks here,
    -- not the consensus part. For now we just override some parameters in the
    -- environment to work around the problem and make sure that none of the
    -- consensus checks in the spec can fail.
    disableConsensusChecks :: Spec.Environment Spec.CHAIN
                           -> Spec.Environment Spec.CHAIN
    disableConsensusChecks ( _currentSlot
                           , utx0
                           , delegators
                           , pparams
                           , k
                           ) = (
          -- Disable 'SlotInTheFuture' failure
          Spec.Slot maxBound
        , utx0
        , delegators
          -- Disable 'TooManyIssuedBlocks' failure
        , pparams { Spec._bkSgnCntT = Spec.BkSgnCntT 1 }
        , k
        )

-- | Construct genesis config from CHAIN environment
--
-- This doesn't make an awful lot of sense, but the abstract spec doesn't /have/
-- a concept of a genesis config, and instead the CHAIN environment fulfills
-- that role. In order to be able to reuse the test generators, we therefore
-- also define a translation in the opposite direction.
fromChainEnv :: Natural -> Spec.Environment Spec.CHAIN -> ColeSpecGenesis
fromChainEnv coleSpecGenesisSlotLength
             ( _currentSlot
             , coleSpecGenesisInitUtxo
             , coleSpecGenesisDelegators
             , coleSpecGenesisInitPParams
             , coleSpecGenesisSecurityParam
             ) = ColeSpecGenesis{..}
