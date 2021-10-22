{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}

-- | Combine all Cole transaction types into single union type
--
-- Intended for qualified import
--
-- > import           Shardagnostic.Consensus.ColeSpec.Ledger.GenTx (ColeSpecGenTx(..), ColeSpecGenTxErr(..))
-- > import qualified Shardagnostic.Consensus.ColeSpec.Ledger.GenTx as GenTx
module Shardagnostic.Consensus.ColeSpec.Ledger.GenTx (
    ColeSpecGenTx (..)
  , ColeSpecGenTxErr (..)
  , apply
  , partition
  ) where

import           Codec.Serialise
import           Control.Monad.Trans.Except
import           GHC.Generics (Generic)

import qualified Cole.Spec.Chain.STS.Rule.Chain as Spec
import qualified Cole.Spec.Ledger.Delegation as Spec
import qualified Cole.Spec.Ledger.UTxO as Spec
import qualified Cole.Spec.Ledger.Update as Spec
import qualified Control.State.Transition as Spec

import           Shardagnostic.Consensus.ColeSpec.Ledger.Genesis
                     (ColeSpecGenesis (..))
import           Shardagnostic.Consensus.ColeSpec.Ledger.Orphans ()
import qualified Shardagnostic.Consensus.ColeSpec.Ledger.Rules as Rules

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | Generalized transaction
--
-- The spec doesn't have a type for this, instead splitting the block body
-- into separate lists
data ColeSpecGenTx =
    ColeSpecGenTxDCert Spec.DCert
  | ColeSpecGenTxTx    Spec.Tx
  | ColeSpecGenTxUProp Spec.UProp
  | ColeSpecGenTxVote  Spec.Vote
  deriving (Show, Generic, Serialise)

-- | Transaction errors
--
-- We don't distinguish these from any other kind of CHAIN failure.
newtype ColeSpecGenTxErr = ColeSpecGenTxErr {
      unColeSpecGenTxErr :: [Spec.PredicateFailure Spec.CHAIN]
    }
  deriving (Show, Generic, Serialise)

{-------------------------------------------------------------------------------
  Functions
-------------------------------------------------------------------------------}

apply :: ColeSpecGenesis
      -> ColeSpecGenTx
      -> Spec.State Spec.CHAIN
      -> Except ColeSpecGenTxErr (Spec.State Spec.CHAIN)
apply cfg = \genTx -> withExcept ColeSpecGenTxErr . go genTx
  where
    go (ColeSpecGenTxDCert dcert) = Rules.liftSDELEG  cfg dcert
    go (ColeSpecGenTxTx    tx   ) = Rules.liftUTXOW   cfg tx
    go (ColeSpecGenTxUProp prop ) = Rules.liftUPIREG  cfg prop
    go (ColeSpecGenTxVote  vote ) = Rules.liftUPIVOTE cfg vote

partition :: [ColeSpecGenTx]
          -> ( [Spec.DCert]
             , [Spec.Tx]
             , [Spec.UProp]
             , [Spec.Vote]
             )
partition = go ([], [], [], [])
  where
    go (ds, ts, us, vs) []     = (reverse ds, reverse ts, reverse us, reverse vs)
    go (ds, ts, us, vs) (g:gs) =
        case g of
          ColeSpecGenTxDCert d -> go (d:ds,   ts,   us,   vs) gs
          ColeSpecGenTxTx    t -> go (  ds, t:ts,   us,   vs) gs
          ColeSpecGenTxUProp u -> go (  ds,   ts, u:us,   vs) gs
          ColeSpecGenTxVote  v -> go (  ds,   ts,   us, v:vs) gs
