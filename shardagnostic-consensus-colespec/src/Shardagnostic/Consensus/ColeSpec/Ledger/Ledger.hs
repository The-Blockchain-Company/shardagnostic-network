{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS -Wno-orphans #-}

module Shardagnostic.Consensus.ColeSpec.Ledger.Ledger (
    ColeSpecLedgerError (..)
  , initColeSpecLedgerState
    -- * Type family instances
  , LedgerState (..)
  , Ticked (..)
  ) where

import           Codec.Serialise
import           Control.Monad.Except
import           GHC.Generics (Generic)
import           NoThunks.Class (AllowThunk (..), NoThunks)

import qualified Cole.Spec.Chain.STS.Rule.Chain as Spec
import qualified Cole.Spec.Ledger.Update as Spec
import qualified Control.State.Transition as Spec

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.Ledger.CommonProtocolParams
import           Shardagnostic.Consensus.Ticked
import           Shardagnostic.Consensus.Util ((..:))

import           Shardagnostic.Consensus.ColeSpec.Ledger.Accessors
import           Shardagnostic.Consensus.ColeSpec.Ledger.Block
import           Shardagnostic.Consensus.ColeSpec.Ledger.Conversions
import           Shardagnostic.Consensus.ColeSpec.Ledger.Genesis (ColeSpecGenesis)
import           Shardagnostic.Consensus.ColeSpec.Ledger.Orphans ()
import qualified Shardagnostic.Consensus.ColeSpec.Ledger.Rules as Rules

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

data instance LedgerState ColeSpecBlock = ColeSpecLedgerState {
      -- | Tip of the ledger (most recently applied block, if any)
      --
      -- The spec state stores the last applied /hash/, but not the /slot/.
      coleSpecLedgerTip :: Maybe SlotNo

      -- | The spec state proper
    , coleSpecLedgerState :: Spec.State Spec.CHAIN
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Serialise)
  deriving NoThunks via AllowThunk (LedgerState ColeSpecBlock)

newtype ColeSpecLedgerError = ColeSpecLedgerError {
      unColeSpecLedgerError :: [Spec.PredicateFailure Spec.CHAIN]
    }
  deriving (Show, Eq)
  deriving NoThunks via AllowThunk ColeSpecLedgerError

type instance LedgerCfg (LedgerState ColeSpecBlock) = ColeSpecGenesis

instance UpdateLedger ColeSpecBlock

initColeSpecLedgerState :: ColeSpecGenesis -> LedgerState ColeSpecBlock
initColeSpecLedgerState cfg = ColeSpecLedgerState {
      coleSpecLedgerTip   = Nothing
    , coleSpecLedgerState = Rules.initStateCHAIN cfg
    }

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

instance GetTip (LedgerState ColeSpecBlock) where
  getTip (ColeSpecLedgerState tip state) = castPoint $
      getColeSpecTip tip state

instance GetTip (Ticked (LedgerState ColeSpecBlock)) where
  getTip (TickedColeSpecLedgerState tip state) = castPoint $
      getColeSpecTip tip state

getColeSpecTip :: Maybe SlotNo -> Spec.State Spec.CHAIN -> Point ColeSpecBlock
getColeSpecTip Nothing     _     = GenesisPoint
getColeSpecTip (Just slot) state = BlockPoint
                                      slot
                                      (getChainStateHash state)

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

data instance Ticked (LedgerState ColeSpecBlock) = TickedColeSpecLedgerState {
      untickedColeSpecLedgerTip :: Maybe SlotNo
    , tickedColeSpecLedgerState :: Spec.State Spec.CHAIN
    }
  deriving stock (Show, Eq)
  deriving NoThunks via AllowThunk (Ticked (LedgerState ColeSpecBlock))

instance IsLedger (LedgerState ColeSpecBlock) where
  type LedgerErr (LedgerState ColeSpecBlock) = ColeSpecLedgerError

  type AuxLedgerEvent (LedgerState ColeSpecBlock) =
    VoidLedgerEvent (LedgerState ColeSpecBlock)

  applyChainTickLedgerResult cfg slot (ColeSpecLedgerState tip state) =
        pureLedgerResult
      $ TickedColeSpecLedgerState {
            untickedColeSpecLedgerTip = tip
          , tickedColeSpecLedgerState = Rules.applyChainTick
                                           cfg
                                           (toColeSpecSlotNo slot)
                                           state
          }

{-------------------------------------------------------------------------------
  Applying blocks
-------------------------------------------------------------------------------}

instance ApplyBlock (LedgerState ColeSpecBlock) ColeSpecBlock where
  applyBlockLedgerResult cfg block (TickedColeSpecLedgerState _tip state) =
        withExcept ColeSpecLedgerError
      $ fmap (pureLedgerResult . ColeSpecLedgerState (Just (blockSlot block)))
      $ -- Note that the CHAIN rule also applies the chain tick. So even
        -- though the ledger we received has already been ticked with
        -- 'applyChainTick', we do it again as part of CHAIN. This is safe, as
        -- it is idempotent. If we wanted to avoid the repeated tick, we would
        -- have to call the subtransitions of CHAIN (except for ticking).
        Rules.liftCHAIN
          cfg
          (coleSpecBlock block)
          state

  reapplyBlockLedgerResult =
      -- The spec doesn't have a "reapply" mode
      dontExpectError ..: applyBlockLedgerResult
    where
      dontExpectError :: Except a b -> b
      dontExpectError mb = case runExcept mb of
        Left  _ -> error "reapplyBlockLedgerResult: unexpected error"
        Right b -> b

{-------------------------------------------------------------------------------
  CommonProtocolParams
-------------------------------------------------------------------------------}

instance CommonProtocolParams ColeSpecBlock where
  maxHeaderSize = fromIntegral . Spec._maxHdrSz . getPParams
  maxTxSize     = fromIntegral . Spec._maxTxSz  . getPParams

getPParams :: LedgerState ColeSpecBlock -> Spec.PParams
getPParams =
      Spec.protocolParameters
    . getChainStateUPIState
    . coleSpecLedgerState
