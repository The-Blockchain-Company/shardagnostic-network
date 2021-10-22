{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE PatternSynonyms         #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Shardagnostic.Consensus.Sophie.Eras (
    -- * Eras based on the Sophie ledger
    EvieEra
  , AurumEra
  , JenEra
  , SophieEra
    -- * Eras instantiated with standard crypto
  , StandardEvie
  , StandardAurum
  , StandardJen
  , StandardSophie
    -- * Sophie-based era
  , SophieBasedEra (..)
  , WrapTx (..)
    -- * Type synonyms for convenience
  , EraCrypto
    -- * Re-exports
  , StandardCrypto
    -- * Exceptions
  , UnexpectedAurumLedgerErrors
  ) where

import           Control.Exception (Exception, throw)
import           Control.Monad.Except
import           Data.Default.Class (Default)
import qualified Data.Set as Set
import           Data.Text (Text)
import           GHC.Records
import           NoThunks.Class (NoThunks)
import           Numeric.Natural (Natural)

import           Bcc.Binary (FromCBOR, ToCBOR)

import           Bcc.Ledger.Evie (EvieEra)
import           Bcc.Ledger.Evie.Translation ()
import           Bcc.Ledger.Aurum (AurumEra)
import           Bcc.Ledger.Aurum.PParams
import qualified Bcc.Ledger.Aurum.Rules.Utxo as Aurum
import qualified Bcc.Ledger.Aurum.Rules.Utxos as Aurum
import qualified Bcc.Ledger.Aurum.Rules.Utxow as Aurum
import qualified Bcc.Ledger.Aurum.Translation as Aurum
import qualified Bcc.Ledger.Aurum.Tx as Aurum
import           Bcc.Ledger.BaseTypes
import qualified Bcc.Ledger.Core as Core
import           Bcc.Ledger.Crypto (StandardCrypto)
import           Bcc.Ledger.Era (Crypto, SupportsSegWit (..))
import qualified Bcc.Ledger.Era as Core
import           Bcc.Ledger.Jen (JenEra)
import           Bcc.Ledger.Jen.Translation ()
import           Bcc.Ledger.Serialization
import           Bcc.Ledger.Sophie (SophieEra)
import           Bcc.Ledger.SophieMA ()
import           Control.State.Transition (State)
import qualified Sophie.Spec.Ledger.API as SL
import qualified Sophie.Spec.Ledger.STS.Ledger as SL
import qualified Sophie.Spec.Ledger.STS.Utxow as SL

import           Shardagnostic.Consensus.Ledger.SupportsMempool
                     (WhetherToIntervene (..))

{-------------------------------------------------------------------------------
  Eras instantiated with standard crypto
-------------------------------------------------------------------------------}

-- | The Sophie era with standard crypto
type StandardSophie = SophieEra StandardCrypto

-- | The Evie era with standard crypto
type StandardEvie = EvieEra StandardCrypto

-- | The Jen era with standard crypto
type StandardJen = JenEra StandardCrypto

-- | The Aurum era with standard crypto
type StandardAurum = AurumEra StandardCrypto

{-------------------------------------------------------------------------------
  Type synonyms for convenience
-------------------------------------------------------------------------------}

-- | The 'Bcc.Ledger.Era.Crypto' type family conflicts with the
-- 'Bcc.Ledger.Crypto.Crypto' class. To avoid having to import one or both
-- of them qualified, define 'EraCrypto' as an alias of the former: /return the
-- crypto used by this era/.
type EraCrypto era = Crypto era

{-------------------------------------------------------------------------------
  Era polymorphism
-------------------------------------------------------------------------------}

-- | The ledger already defines 'SL.SophieBasedEra' as /the/ top-level
-- constraint on an era, however, consensus often needs some more functionality
-- than the ledger currently provides.
--
-- Either the functionality shouldn't or can't live in the ledger, in which case
-- it can be part and remain part of 'SophieBasedEra'. Or, the functionality
-- /should/ live in the ledger, but hasn't yet been added to the ledger, or it
-- hasn't yet been propagated to this repository, in which case it can be added
-- to this class until that is the case.
--
-- By having the same name as the class defined in ledger, we can, if this class
-- becomes redundant, switch to the ledger-defined one without having to update
-- all the code using it. We can just export the right one from this module.
--
-- TODO Currently we include some constraints on the update state which are
-- needed to determine the hard fork point. In the future this should be
-- replaced with an appropriate API - see
-- https://github.com/The-Blockchain-Company/shardagnostic-network/issues/2890
class ( SL.SophieBasedEra era

      , State (Core.EraRule "PPUP" era) ~ SL.PPUPState era
      , Default (State (Core.EraRule "PPUP" era))

      , HasField "_maxBHSize" (Core.PParams era) Natural
      , HasField "_maxTxSize" (Core.PParams era) Natural
      , HasField "_a0" (Core.PParams era) NonNegativeInterval
      , HasField "_nOpt" (Core.PParams era) Natural
      , HasField "_rho" (Core.PParams era) UnitInterval
      , HasField "_tau" (Core.PParams era) UnitInterval

      , FromCBOR (Core.PParams era)
      , ToCBOR (Core.PParams era)

      , HasField "_protocolVersion" (Core.PParamsDelta era) (SL.StrictMaybe SL.ProtVer)
      , FromCBOR (Core.PParamsDelta era)

      , SL.AdditionalGenesisConfig era ~ Core.TranslationContext era
      , ToCBORGroup (TxSeq era)

      , NoThunks (Core.TranslationContext era)

      , ToCBOR (Core.Witnesses era)

      ) => SophieBasedEra era where

  -- | Return the name of the Sophie-based era, e.g., @"Sophie"@, @"Evie"@,
  -- etc.
  sophieBasedEraName :: proxy era -> Text

  applySophieBasedTx ::
       SL.Globals
    -> SL.LedgerEnv    era
    -> SL.MempoolState era
    -> WhetherToIntervene
    -> Core.Tx           era
    -> Except
         (SL.ApplyTxError era)
         ( SL.MempoolState era
         , SL.Validated (Core.Tx era)
         )

-- | The default implementation of 'applySophieBasedTx', a thin wrapper around
-- 'SL.applyTx'
defaultApplySophieBasedTx ::
     SophieBasedEra era
  => SL.Globals
  -> SL.LedgerEnv    era
  -> SL.MempoolState era
  -> WhetherToIntervene
  -> Core.Tx         era
  -> Except
       (SL.ApplyTxError era)
       ( SL.MempoolState era
       , SL.Validated (Core.Tx era)
       )
defaultApplySophieBasedTx globals ledgerEnv mempoolState _wti tx =
    SL.applyTx
      globals
      ledgerEnv
      mempoolState
      tx

instance SL.OptimumCrypto c => SophieBasedEra (SophieEra c) where
  sophieBasedEraName _ = "Sophie"

  applySophieBasedTx = defaultApplySophieBasedTx

instance SL.OptimumCrypto c => SophieBasedEra (EvieEra c) where
  sophieBasedEraName _ = "Evie"

  applySophieBasedTx = defaultApplySophieBasedTx

instance SL.OptimumCrypto c => SophieBasedEra (JenEra c) where
  sophieBasedEraName _ = "Jen"

  applySophieBasedTx = defaultApplySophieBasedTx

instance SL.OptimumCrypto c => SophieBasedEra (AurumEra c) where
  sophieBasedEraName _ = "Aurum"

  applySophieBasedTx globals ledgerEnv mempoolState wti tx = do
      (mempoolState', vtx) <-
          (`catchError` handler)
        $ defaultApplySophieBasedTx
            globals
            ledgerEnv
            mempoolState
            wti
            tx

      pure (mempoolState', vtx)
    where
      nubOrd = Set.toList . Set.fromList

      handler e = case (wti, e) of
        (DoNotIntervene, SL.ApplyTxError errs)
          | flag:flags <- nubOrd [b | IncorrectClaimedFlag b <- errs] ->
            if not (null flags)
            then throw $ UnexpectedAurumLedgerErrors (flag:flags)
            else
            -- rectify the flag and include the transaction
            --
            -- This either lets the ledger punish the script author for sending
            -- a bad script or else prevents our peer's buggy script validator
            -- from preventing inclusion of a valid script.
            --
            -- TODO 'applyTx' et al needs to include a return value indicating
            -- whether we took this branch; it's a reason to disconnect from
            -- the peer who sent us the incorrect flag (ie Issue #3276)
            defaultApplySophieBasedTx
              globals
              ledgerEnv
              mempoolState
              wti
              tx{Aurum.isValid = Aurum.IsValid (not flag)}
        _ -> throwError e
               -- reject the transaction, protecting the local wallet

-- not exported
--
-- The ledger failure we see when the transaction's claimed 'IsValid'
-- flag was incorrect
pattern IncorrectClaimedFlag ::
     Bool
  -> SL.PredicateFailure (Core.EraRule "LEDGER" (AurumEra c))
pattern IncorrectClaimedFlag claimedFlag <-
    SL.UtxowFailure
      (Aurum.WrappedSophieEraFailure
        (SL.UtxoFailure
          (Aurum.UtxosFailure
            (Aurum.ValidationTagMismatch
              (Aurum.IsValid claimedFlag)
              _validationErrs
      ))))

-- | The ledger responded with Aurum errors we were not expecting
data UnexpectedAurumLedgerErrors =
    -- | We received more than one 'Aurum.ValidationTagMismatch'
    --
    -- The exception lists the 'Aurum.IsValid' flags we saw.
    UnexpectedAurumLedgerErrors [Bool]
  deriving (Show, Exception)

{-------------------------------------------------------------------------------
  Tx family wrapper
-------------------------------------------------------------------------------}

-- | Wrapper for partially applying the 'Tx' type family
--
-- For generality, Consensus uses that type family as eg the index of
-- 'Core.TranslateEra'. We thus need to partially apply it.
--
-- @bcc-ledger-specs@ also declares such a newtype, but currently it's only
-- defined in the Aurum translation module, which seems somewhat inappropriate
-- to use for previous eras. Also, we use a @Wrap@ prefix in Consensus. Hence
-- this minor mediating definition. TODO I'm not even fully persuading myself
-- with this justification.
newtype WrapTx era = WrapTx {unwrapTx :: Core.Tx era}

instance SophieBasedEra (EvieEra c) => Core.TranslateEra (EvieEra c) WrapTx where
  type TranslationError (EvieEra c) WrapTx = Core.TranslationError (EvieEra c) SL.Tx
  translateEra ctxt = fmap WrapTx . Core.translateEra ctxt . unwrapTx

instance SophieBasedEra (JenEra c) => Core.TranslateEra (JenEra c) WrapTx where
  type TranslationError (JenEra c) WrapTx = Core.TranslationError (JenEra c) SL.Tx
  translateEra ctxt = fmap WrapTx . Core.translateEra ctxt . unwrapTx

instance SophieBasedEra (AurumEra c) => Core.TranslateEra (AurumEra c) WrapTx where
  type TranslationError (AurumEra c) WrapTx = Core.TranslationError (AurumEra c) Aurum.Tx
  translateEra ctxt =
        fmap (WrapTx . Aurum.unTx)
      . Core.translateEra @(AurumEra c) ctxt
      . Aurum.Tx . unwrapTx
