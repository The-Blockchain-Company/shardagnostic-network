{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Cole.Generators (
    RegularBlock (..)
  , epochSlots
  , k
  , protocolMagicId
  ) where

import           Control.Monad (replicateM)
import           Data.Coerce (coerce)
import qualified Data.Map.Strict as Map

import           Bcc.Binary (fromCBOR, toCBOR)
import           Bcc.Chain.Block (ABlockOrBoundary (..),
                     ABlockOrBoundaryHdr (..))
import qualified Bcc.Chain.Block as CC.Block
import qualified Bcc.Chain.Cole.API as API
import           Bcc.Chain.Common (KeyHash)
import qualified Bcc.Chain.Delegation as CC.Del
import qualified Bcc.Chain.Delegation.Validation.Activation as CC.Act
import qualified Bcc.Chain.Delegation.Validation.Interface as CC.DI
import qualified Bcc.Chain.Delegation.Validation.Scheduling as CC.Sched
import qualified Bcc.Chain.Genesis as CC.Genesis
import           Bcc.Chain.Slotting (EpochNumber, EpochSlots (..),
                     SlotNumber)
import qualified Bcc.Chain.UTxO as CC.UTxO
import qualified Bcc.Chain.Update as CC.Update
import qualified Bcc.Chain.Update.Validation.Interface as CC.UPI
import qualified Bcc.Chain.Update.Validation.Registration as CC.Reg
import           Bcc.Crypto (ProtocolMagicId (..))
import           Bcc.Crypto.Hashing (Hash)

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Config.SecurityParam
import           Shardagnostic.Consensus.HeaderValidation (AnnTip (..))
import           Shardagnostic.Consensus.Ledger.SupportsMempool (GenTxId)
import           Shardagnostic.Consensus.Protocol.PBFT.State (PBftState)
import qualified Shardagnostic.Consensus.Protocol.PBFT.State as PBftState

import           Shardagnostic.Consensus.Cole.Ledger
import           Shardagnostic.Consensus.Cole.Protocol

import           Test.QuickCheck hiding (Result)
import           Test.QuickCheck.Hedgehog (hedgehog)

import qualified Test.Bcc.Chain.Block.Gen as CC
import qualified Test.Bcc.Chain.Common.Gen as CC
import qualified Test.Bcc.Chain.Delegation.Gen as CC
import qualified Test.Bcc.Chain.MempoolPayload.Gen as CC
import qualified Test.Bcc.Chain.Slotting.Gen as CC
import qualified Test.Bcc.Chain.UTxO.Gen as CC
import qualified Test.Bcc.Chain.Update.Gen as UG
import qualified Test.Bcc.Crypto.Gen as CC

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Roundtrip (Coherent (..),
                     SomeResult (..), WithVersion (..))

{-------------------------------------------------------------------------------
  Generators
-------------------------------------------------------------------------------}

-- | Matches that from the 'CC.dummyConfig'
k :: SecurityParam
k = SecurityParam 10

-- | Matches that from the 'CC.dummyConfig'
epochSlots :: EpochSlots
epochSlots = EpochSlots 100

protocolMagicId :: ProtocolMagicId
protocolMagicId = ProtocolMagicId 100

-- | A 'ColeBlock' that is never an EBB.
newtype RegularBlock = RegularBlock { unRegularBlock :: ColeBlock }
  deriving (Eq, Show)

instance Arbitrary RegularBlock where
  arbitrary =
    RegularBlock .annotateColeBlock epochSlots <$>
    hedgehog (CC.genBlock protocolMagicId epochSlots)

instance Arbitrary ColeBlock where
  arbitrary = getCoherent <$> arbitrary

instance Arbitrary (Coherent ColeBlock) where
  arbitrary = Coherent <$> frequency
      [ (3, genBlock)
      , (1, genBoundaryBlock)
      ]
    where
      genBlock :: Gen ColeBlock
      genBlock = unRegularBlock <$> arbitrary
      genBoundaryBlock :: Gen ColeBlock
      genBoundaryBlock =
        mkColeBlock epochSlots . ABOBBoundary . API.reAnnotateBoundary protocolMagicId <$>
        hedgehog (CC.genBoundaryBlock)

instance Arbitrary (Header ColeBlock) where
  arbitrary = frequency
      [ (3, genHeader)
      , (1, genBoundaryHeader)
      ]
    where
      genHeader :: Gen (Header ColeBlock)
      genHeader = do
        blockSize <- arbitrary
        flip (mkColeHeader epochSlots) blockSize . ABOBBlockHdr .
          API.reAnnotateUsing
            (CC.Block.toCBORHeader epochSlots)
            (CC.Block.fromCBORAHeader epochSlots) <$>
          hedgehog (CC.genHeader protocolMagicId epochSlots)

      genBoundaryHeader :: Gen (Header ColeBlock)
      genBoundaryHeader = do
        blockSize <- arbitrary
        flip (mkColeHeader epochSlots) blockSize . ABOBBoundaryHdr .
          API.reAnnotateUsing
            (CC.Block.toCBORABoundaryHeader protocolMagicId)
            CC.Block.fromCBORABoundaryHeader <$>
          hedgehog CC.genBoundaryHeader

instance Arbitrary (Hash a) where
  arbitrary = coerce <$> hedgehog CC.genTextHash

instance Arbitrary ColeHash where
  arbitrary = ColeHash <$> arbitrary

instance Arbitrary KeyHash where
  arbitrary = hedgehog CC.genKeyHash

instance Arbitrary (GenTx ColeBlock) where
  arbitrary =
    fromMempoolPayload . API.reAnnotateUsing toCBOR fromCBOR <$>
    hedgehog (CC.genMempoolPayload protocolMagicId)

instance Arbitrary (GenTxId ColeBlock) where
  arbitrary = oneof
      [ ColeTxId             <$> hedgehog CC.genTxId
      , ColeDlgId            <$> hedgehog genCertificateId
      , ColeUpdateProposalId <$> hedgehog (UG.genUpId protocolMagicId)
      , ColeUpdateVoteId     <$> hedgehog genUpdateVoteId
      ]
    where
      genCertificateId = CC.genAbstractHash (CC.genCertificate protocolMagicId)
      genUpdateVoteId  = CC.genAbstractHash (UG.genVote protocolMagicId)

instance Arbitrary API.ApplyMempoolPayloadErr where
  arbitrary = oneof
    [ API.MempoolTxErr  <$> hedgehog CC.genUTxOValidationError
    , API.MempoolDlgErr <$> hedgehog CC.genError
    -- TODO there is no generator for
    -- Bcc.Chain.Update.Validation.Interface.Error and we can't write one
    -- either because the different Error types it wraps are not exported.
    -- , MempoolUpdateProposalErr <$> arbitrary
    -- , MempoolUpdateVoteErr     <$> arbitrary
    ]

instance Arbitrary (SomeSecond BlockQuery ColeBlock) where
  arbitrary = pure $ SomeSecond GetUpdateInterfaceState

instance Arbitrary EpochNumber where
  arbitrary = hedgehog CC.genEpochNumber

instance Arbitrary SlotNumber where
  arbitrary = hedgehog CC.genSlotNumber

instance Arbitrary CC.Update.ApplicationName where
  arbitrary = hedgehog UG.genApplicationName

instance Arbitrary CC.Update.SystemTag where
  arbitrary = hedgehog UG.genSystemTag

instance Arbitrary CC.Update.InstallerHash where
  arbitrary = hedgehog UG.genInstallerHash

instance Arbitrary CC.Update.ProtocolVersion where
  arbitrary = hedgehog UG.genProtocolVersion

instance Arbitrary CC.Update.ProtocolParameters where
  arbitrary = hedgehog UG.genProtocolParameters

instance Arbitrary CC.Update.SoftwareVersion where
  arbitrary = hedgehog UG.genSoftwareVersion

instance Arbitrary CC.Reg.ProtocolUpdateProposal where
  arbitrary = CC.Reg.ProtocolUpdateProposal
    <$> arbitrary
    <*> arbitrary

instance Arbitrary CC.Reg.SoftwareUpdateProposal where
  arbitrary = CC.Reg.SoftwareUpdateProposal
    <$> arbitrary
    <*> arbitrary

instance Arbitrary CC.Reg.ApplicationVersion where
  arbitrary = CC.Reg.ApplicationVersion
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary CC.UPI.State where
  arbitrary = CC.UPI.State
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure mempty -- TODO CandidateProtocolUpdate's constructor is not exported
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure mempty -- TODO Endorsement is not exported
    <*> arbitrary

instance Arbitrary CC.Genesis.GenesisHash where
  arbitrary = CC.Genesis.GenesisHash <$> arbitrary

instance Arbitrary CC.UTxO.UTxO where
  arbitrary = hedgehog CC.genUTxO

instance Arbitrary CC.Act.State where
  arbitrary = CC.Act.State
    <$> arbitrary
    <*> arbitrary

instance Arbitrary CC.Sched.ScheduledDelegation where
  arbitrary = CC.Sched.ScheduledDelegation
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary CC.Sched.State where
  arbitrary = CC.Sched.State
    <$> arbitrary
    <*> arbitrary

instance Arbitrary CC.DI.State where
  arbitrary = CC.DI.State
    <$> arbitrary
    <*> arbitrary

instance Arbitrary CC.Block.ChainValidationState where
  arbitrary = CC.Block.ChainValidationState
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary ColeNodeToNodeVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ColeNodeToClientVersion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CC.Del.Map where
  arbitrary = CC.Del.fromList <$> arbitrary

instance Arbitrary ColeTransition where
  arbitrary = ColeTransitionInfo . Map.fromList <$> arbitrary

instance Arbitrary (LedgerState ColeBlock) where
  arbitrary = ColeLedgerState <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (TipInfoIsEBB ColeBlock) where
  arbitrary = TipInfoIsEBB <$> arbitrary <*> elements [IsEBB, IsNotEBB]

instance Arbitrary (AnnTip ColeBlock) where
  arbitrary = AnnTip
    <$> (SlotNo  <$> arbitrary)
    <*> (BlockNo <$> arbitrary)
    <*> arbitrary

instance Arbitrary (PBftState PBftColeCrypto) where
  arbitrary = do
      slots <- choose (0, 10)
      keys  <- replicateM 3 arbitrary
      let signers = zipWith PBftState.PBftSigner (map SlotNo [0..slots]) (cycle keys)
      return $ PBftState.fromList signers

instance Arbitrary (SomeResult ColeBlock) where
  arbitrary = SomeResult GetUpdateInterfaceState  <$> arbitrary

{-------------------------------------------------------------------------------
  Versioned generators for serialisation
-------------------------------------------------------------------------------}

-- | We only have to be careful about headers with ColeNodeToNodeVersion1,
-- where we will have a fake block size hint.
instance Arbitrary (WithVersion ColeNodeToNodeVersion (Header ColeBlock)) where
  arbitrary = do
    version <- arbitrary
    hdr     <- arbitrary
    let hdr' = case version of
          ColeNodeToNodeVersion1 ->
            hdr { coleHeaderBlockSizeHint = fakeColeBlockSizeHint }
          ColeNodeToNodeVersion2 ->
            hdr
    return (WithVersion version hdr')

instance Arbitrary (WithVersion ColeNodeToNodeVersion (SomeSecond (NestedCtxt Header) ColeBlock)) where
  arbitrary = do
      version <- arbitrary
      size    <- case version of
                   ColeNodeToNodeVersion1 -> return fakeColeBlockSizeHint
                   ColeNodeToNodeVersion2 -> arbitrary
      ctxt    <- elements [
                     SomeSecond . NestedCtxt $ CtxtColeRegular  size
                   , SomeSecond . NestedCtxt $ CtxtColeBoundary size
                   ]
      return (WithVersion version ctxt)
