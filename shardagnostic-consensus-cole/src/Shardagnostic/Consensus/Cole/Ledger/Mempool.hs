{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Cole mempool integration
module Shardagnostic.Consensus.Cole.Ledger.Mempool (
    -- * Mempool integration
    GenTx (..)
  , TxId (..)
  , Validated (..)
    -- * Transaction IDs
  , coleIdDlg
  , coleIdProp
  , coleIdTx
  , coleIdVote
    -- * Serialisation
  , decodeColeApplyTxError
  , decodeColeGenTx
  , decodeColeGenTxId
  , encodeColeApplyTxError
  , encodeColeGenTx
  , encodeColeGenTxId
    -- * Low-level API (primarily for testing)
  , fromMempoolPayload
  , toMempoolPayload
    -- * Auxiliary functions
  , countColeGenTxs
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as CBOR
import           Control.Monad.Except
import           Data.ByteString (ByteString)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Maybe (maybeToList)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (InspectHeapNamed (..), NoThunks (..))

import           Bcc.Binary (ByteSpan, DecoderError (..), FromCBOR (..),
                     ToCBOR (..), enforceSize, fromCBOR, serialize, slice,
                     toCBOR, unsafeDeserialize)
import           Bcc.Crypto (hashDecoded)
import           Bcc.Prelude (cborError)

import qualified Bcc.Chain.Block as CC
import qualified Bcc.Chain.Cole.API as CC
import qualified Bcc.Chain.Delegation as Delegation
import qualified Bcc.Chain.MempoolPayload as CC
import qualified Bcc.Chain.UTxO as Utxo
import qualified Bcc.Chain.Update as Update
import qualified Bcc.Chain.ValidationMode as CC

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.Ledger.SupportsMempool
import           Shardagnostic.Consensus.Util (ShowProxy (..))
import           Shardagnostic.Consensus.Util.Condense

import           Shardagnostic.Consensus.Cole.Ledger.Block
import           Shardagnostic.Consensus.Cole.Ledger.Conversions (toColeSlotNo)
import           Shardagnostic.Consensus.Cole.Ledger.Ledger
import           Shardagnostic.Consensus.Cole.Ledger.Orphans ()
import           Shardagnostic.Consensus.Cole.Ledger.Serialisation
                     (coleBlockEncodingOverhead)
import           Shardagnostic.Consensus.Mempool.TxLimits

{-------------------------------------------------------------------------------
  TxLimits
-------------------------------------------------------------------------------}

instance TxLimits ColeBlock where
  type TxMeasure ColeBlock = ByteSize
  txMeasure        = ByteSize . txInBlockSize . txForgetValidated
  txsBlockCapacity = ByteSize . txsMaxBytes

{-------------------------------------------------------------------------------
  Transactions
-------------------------------------------------------------------------------}

-- | Generalized transactions in Cole
--
-- This is effectively the same as 'CC.AMempoolPayload' but we cache the
-- transaction ID (a hash).
data instance GenTx ColeBlock
  = ColeTx             !Utxo.TxId                !(Utxo.ATxAux             ByteString)
  | ColeDlg            !Delegation.CertificateId !(Delegation.ACertificate ByteString)
  | ColeUpdateProposal !Update.UpId              !(Update.AProposal        ByteString)
  | ColeUpdateVote     !Update.VoteId            !(Update.AVote            ByteString)
  deriving (Eq, Generic)
  deriving NoThunks via InspectHeapNamed "GenTx ColeBlock" (GenTx ColeBlock)

instance ShowProxy (GenTx ColeBlock) where

newtype instance Validated (GenTx ColeBlock) = ValidatedColeTx {
      forgetValidatedColeTx :: GenTx ColeBlock
    }
  deriving (Eq, Generic)
  deriving anyclass (NoThunks)

type instance ApplyTxErr ColeBlock = CC.ApplyMempoolPayloadErr

-- orphaned instance
instance ShowProxy CC.ApplyMempoolPayloadErr where

instance LedgerSupportsMempool ColeBlock where
  -- Check that the annotation is the canonical encoding. This is currently
  -- enforced by 'decodeColeGenTx', see its docstring for more context.
  txInvariant tx =
      CC.mempoolPayloadRecoverBytes tx' == CC.mempoolPayloadReencode tx'
    where
      tx' = toMempoolPayload tx

  applyTx cfg _wti slot tx st =
          (\st' -> (st', ValidatedColeTx tx))
      <$> applyColeGenTx validationMode cfg slot tx st
    where
      validationMode = CC.ValidationMode CC.BlockValidation Utxo.TxValidation

  reapplyTx cfg slot vtx st =
      applyColeGenTx validationMode cfg slot (forgetValidatedColeTx vtx) st
    where
      validationMode = CC.ValidationMode CC.NoBlockValidation Utxo.TxValidationNoCrypto

  txsMaxBytes st =
    CC.getMaxBlockSize (tickedColeLedgerState st) - coleBlockEncodingOverhead

  txInBlockSize =
      fromIntegral
    . Strict.length
    . CC.mempoolPayloadRecoverBytes
    . toMempoolPayload

  txForgetValidated = forgetValidatedColeTx

data instance TxId (GenTx ColeBlock)
  = ColeTxId             !Utxo.TxId
  | ColeDlgId            !Delegation.CertificateId
  | ColeUpdateProposalId !Update.UpId
  | ColeUpdateVoteId     !Update.VoteId
  deriving (Eq, Ord)
  deriving NoThunks via InspectHeapNamed "TxId (GenTx ColeBlock)" (TxId (GenTx ColeBlock))

instance ShowProxy (TxId (GenTx ColeBlock)) where

instance HasTxId (GenTx ColeBlock) where
  txId (ColeTx             i _) = ColeTxId             i
  txId (ColeDlg            i _) = ColeDlgId            i
  txId (ColeUpdateProposal i _) = ColeUpdateProposalId i
  txId (ColeUpdateVote     i _) = ColeUpdateVoteId     i

instance HasTxs ColeBlock where
  extractTxs blk = case coleBlockRaw blk of
    -- EBBs don't contain transactions
    CC.ABOBBoundary _ebb    -> []
    CC.ABOBBlock regularBlk -> fromMempoolPayload <$>
        maybeToList proposal <> votes <> dlgs <> txs
      where
        body = CC.blockBody regularBlk

        txs      = CC.MempoolTx             <$> Utxo.aUnTxPayload      (CC.bodyTxPayload     body)
        proposal = CC.MempoolUpdateProposal <$> Update.payloadProposal (CC.bodyUpdatePayload body)
        votes    = CC.MempoolUpdateVote     <$> Update.payloadVotes    (CC.bodyUpdatePayload body)
        dlgs     = CC.MempoolDlg            <$> Delegation.getPayload  (CC.bodyDlgPayload    body)

{-------------------------------------------------------------------------------
  Conversion to and from 'AMempoolPayload'
-------------------------------------------------------------------------------}

toMempoolPayload :: GenTx ColeBlock -> CC.AMempoolPayload ByteString
toMempoolPayload = go
  where
    -- Just extract the payload @p@
    go :: GenTx ColeBlock -> CC.AMempoolPayload ByteString
    go (ColeTx             _ p) = CC.MempoolTx             p
    go (ColeDlg            _ p) = CC.MempoolDlg            p
    go (ColeUpdateProposal _ p) = CC.MempoolUpdateProposal p
    go (ColeUpdateVote     _ p) = CC.MempoolUpdateVote     p

fromMempoolPayload :: CC.AMempoolPayload ByteString -> GenTx ColeBlock
fromMempoolPayload = go
  where
    -- Bundle the payload @p@ with its ID
    go :: CC.AMempoolPayload ByteString -> GenTx ColeBlock
    go (CC.MempoolTx             p) = ColeTx             (coleIdTx   p) p
    go (CC.MempoolDlg            p) = ColeDlg            (coleIdDlg  p) p
    go (CC.MempoolUpdateProposal p) = ColeUpdateProposal (coleIdProp p) p
    go (CC.MempoolUpdateVote     p) = ColeUpdateVote     (coleIdVote p) p

{-------------------------------------------------------------------------------
  Auxiliary: transaction IDs
-------------------------------------------------------------------------------}

-- TODO: move to bcc-ledger-cole (bcc-ledger-cole#581)
coleIdTx :: Utxo.ATxAux ByteString -> Utxo.TxId
coleIdTx = hashDecoded . Utxo.aTaTx

coleIdDlg :: Delegation.ACertificate ByteString -> Delegation.CertificateId
coleIdDlg = Delegation.recoverCertificateId

coleIdProp :: Update.AProposal ByteString -> Update.UpId
coleIdProp = Update.recoverUpId

coleIdVote :: Update.AVote ByteString -> Update.VoteId
coleIdVote = Update.recoverVoteId

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Condense (GenTx ColeBlock) where
  condense = condense . toMempoolPayload

instance Condense (GenTxId ColeBlock) where
  condense (ColeTxId             i) = condense i
  condense (ColeDlgId            i) = condense i
  condense (ColeUpdateProposalId i) = condense i
  condense (ColeUpdateVoteId     i) = condense i

instance Show (GenTx ColeBlock) where
  show = condense

instance Show (Validated (GenTx ColeBlock)) where
  show vtx = "Validated-" <> condense (forgetValidatedColeTx vtx)

instance Show (GenTxId ColeBlock) where
  show = condense

{-------------------------------------------------------------------------------
  Applying transactions
-------------------------------------------------------------------------------}

applyColeGenTx :: CC.ValidationMode
                -> LedgerConfig ColeBlock
                -> SlotNo
                -> GenTx ColeBlock
                -> TickedLedgerState ColeBlock
                -> Except (ApplyTxErr ColeBlock) (TickedLedgerState ColeBlock)
applyColeGenTx validationMode cfg slot genTx st =
    (\state -> st {tickedColeLedgerState = state}) <$>
      CC.applyMempoolPayload
        validationMode
        cfg
        (toColeSlotNo slot)
        (toMempoolPayload genTx)
        (tickedColeLedgerState st)

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

encodeColeGenTx :: GenTx ColeBlock -> Encoding
encodeColeGenTx genTx = toCBOR (toMempoolPayload genTx)

-- | The 'ByteString' annotation will be the canonical encoding.
--
-- While the new implementation does not care about canonical encodings, the
-- old one does. When a generalised transaction arrives that is not in its
-- canonical encoding (only the 'CC.UTxO.ATxAux' of the 'ColeTx' can be
-- produced by nodes that are not under our control), the old implementation
-- will reject it. Therefore, we need to reject them too. See #905.
--
-- We use the ledger to check for canonical encodings: the ledger will check
-- whether the signed hash of the transaction (in the case of a
-- 'CC.UTxO.ATxAux', the transaction witness) matches the annotated
-- bytestring. Is therefore __important__ that the annotated bytestring be the
-- /canonical/ encoding, not the /original, possibly non-canonical/ encoding.
decodeColeGenTx :: Decoder s (GenTx ColeBlock)
decodeColeGenTx = fromMempoolPayload . canonicalise <$> fromCBOR
  where
    -- Fill in the 'ByteString' annotation with a canonical encoding of the
    -- 'GenTx'. We must reserialise the deserialised 'GenTx' to be sure we
    -- have the canonical one. We don't have access to the original
    -- 'ByteString' anyway, so having to reserialise here gives us a
    -- 'ByteString' we can use.
    canonicalise :: CC.AMempoolPayload ByteSpan
                 -> CC.AMempoolPayload ByteString
    canonicalise mp = Lazy.toStrict . slice canonicalBytes <$> mp'
      where
        canonicalBytes = serialize (void mp)
        -- 'unsafeDeserialize' cannot fail, since we just 'serialize'd it.
        -- Note that we cannot reuse @mp@, as its 'ByteSpan' might differ from
        -- the canonical encoding's 'ByteSpan'.
        mp'            = unsafeDeserialize canonicalBytes

encodeColeGenTxId :: GenTxId ColeBlock -> Encoding
encodeColeGenTxId genTxId = mconcat [
      CBOR.encodeListLen 2
    , case genTxId of
        ColeTxId             i -> toCBOR (0 :: Word8) <> toCBOR i
        ColeDlgId            i -> toCBOR (1 :: Word8) <> toCBOR i
        ColeUpdateProposalId i -> toCBOR (2 :: Word8) <> toCBOR i
        ColeUpdateVoteId     i -> toCBOR (3 :: Word8) <> toCBOR i
    ]

decodeColeGenTxId :: Decoder s (GenTxId ColeBlock)
decodeColeGenTxId = do
    enforceSize "GenTxId (ColeBlock cfg)" 2
    CBOR.decodeWord8 >>= \case
      0   -> ColeTxId             <$> fromCBOR
      1   -> ColeDlgId            <$> fromCBOR
      2   -> ColeUpdateProposalId <$> fromCBOR
      3   -> ColeUpdateVoteId     <$> fromCBOR
      tag -> cborError $ DecoderErrorUnknownTag "GenTxId (ColeBlock cfg)" tag

encodeColeApplyTxError :: ApplyTxErr ColeBlock -> Encoding
encodeColeApplyTxError = toCBOR

decodeColeApplyTxError :: Decoder s (ApplyTxErr ColeBlock)
decodeColeApplyTxError = fromCBOR

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

-- | Count all (generalized) transactions in the block
countColeGenTxs :: ColeBlock -> Word64
countColeGenTxs = fromIntegral . length . extractTxs
