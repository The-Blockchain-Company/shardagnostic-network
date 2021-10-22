{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Test.Consensus.Cole.Examples (
    -- * Setup
    cfg
  , codecConfig
  , leaderCredentials
  , ledgerConfig
  , secParam
  , windowSize
    -- * Examples
  , exampleApplyTxErr
  , exampleChainDepState
  , exampleExtLedgerState
  , exampleGenTx
  , exampleGenTxId
  , exampleHeaderHash
  , exampleHeaderState
  , exampleLedgerState
  , examples
  ) where

import           Control.Monad.Except (runExcept)
import qualified Data.Map.Strict as Map

import qualified Bcc.Chain.Block as CC.Block
import qualified Bcc.Chain.Cole.API as CC
import qualified Bcc.Chain.Common as CC
import qualified Bcc.Chain.UTxO as CC
import qualified Bcc.Chain.Update.Validation.Interface as CC.UPI

import           Shardagnostic.Network.Block (Serialised (..))

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Config
import           Shardagnostic.Consensus.HeaderValidation
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.Ledger.Extended
import qualified Shardagnostic.Consensus.Mempool.TxLimits as TxLimits
import           Shardagnostic.Consensus.NodeId
import           Shardagnostic.Consensus.Protocol.Abstract
import           Shardagnostic.Consensus.Protocol.PBFT
import qualified Shardagnostic.Consensus.Protocol.PBFT.State as S
import           Shardagnostic.Consensus.Storage.Serialisation

import           Shardagnostic.Consensus.Cole.Crypto.DSIGN (SignKeyDSIGN (..))
import           Shardagnostic.Consensus.Cole.Ledger
import           Shardagnostic.Consensus.Cole.Node (ColeLeaderCredentials (..))

import           Test.Util.Serialisation.Golden (Labelled, labelled, unlabelled)
import qualified Test.Util.Serialisation.Golden as Golden
import           Test.Util.Serialisation.Roundtrip (SomeResult (..))

import qualified Test.Bcc.Chain.Common.Example as CC
import qualified Test.Bcc.Chain.Genesis.Dummy as CC
import qualified Test.Bcc.Chain.UTxO.Example as CC
import qualified Test.Bcc.Chain.Update.Example as CC

import           Test.ThreadNet.Infra.Cole.ProtocolInfo (mkLeaderCredentials)

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

-- | Note that we must use the same value for the 'SecurityParam' as for the
-- 'S.WindowSize', because 'decodeColeChainDepState' only takes the
-- 'SecurityParam' and uses it as the basis for the 'S.WindowSize'.
secParam :: SecurityParam
secParam = SecurityParam 2

windowSize :: S.WindowSize
windowSize = S.WindowSize 2

cfg :: BlockConfig ColeBlock
cfg = ColeConfig {
      coleGenesisConfig   = CC.dummyConfig
    , coleProtocolVersion = CC.exampleProtocolVersion
    , coleSoftwareVersion = CC.exampleSoftwareVersion
    }

codecConfig :: CodecConfig ColeBlock
codecConfig = mkColeCodecConfig CC.dummyConfig

ledgerConfig :: LedgerConfig ColeBlock
ledgerConfig = CC.dummyConfig

leaderCredentials :: ColeLeaderCredentials
leaderCredentials =
    mkLeaderCredentials
      CC.dummyConfig
      CC.dummyGeneratedSecrets
      (CoreNodeId 0)

{-------------------------------------------------------------------------------
  Examples
-------------------------------------------------------------------------------}

examples :: Golden.Examples ColeBlock
examples = Golden.Examples {
      exampleBlock            = regularAndEBB exampleBlock            exampleEBB
    , exampleSerialisedBlock  = regularAndEBB exampleSerialisedBlock  exampleSerialisedEBB
    , exampleHeader           = regularAndEBB exampleHeader           exampleEBBHeader
    , exampleSerialisedHeader = regularAndEBB exampleSerialisedHeader exampleSerialisedEBBHeader
    , exampleHeaderHash       = unlabelled exampleHeaderHash
    , exampleGenTx            = unlabelled exampleGenTx
    , exampleGenTxId          = unlabelled exampleGenTxId
    , exampleApplyTxErr       = unlabelled exampleApplyTxErr
    , exampleQuery            = unlabelled exampleQuery
    , exampleResult           = unlabelled exampleResult
    , exampleAnnTip           = unlabelled exampleAnnTip
    , exampleLedgerState      = unlabelled exampleLedgerState
    , exampleChainDepState    = unlabelled exampleChainDepState
    , exampleExtLedgerState   = unlabelled exampleExtLedgerState
    }
  where
    regularAndEBB :: a -> a -> Labelled a
    regularAndEBB regular ebb = labelled [("regular", regular), ("EBB", ebb)]

    exampleQuery  = SomeSecond GetUpdateInterfaceState
    exampleResult = SomeResult GetUpdateInterfaceState exampleUPIState

exampleBlock :: ColeBlock
exampleBlock =
    forgeRegularBlock
      cfg
      (TxLimits.mkOverrides TxLimits.noOverridesMeasure)
      (BlockNo 1)
      (SlotNo 1)
      (applyChainTick ledgerConfig (SlotNo 1) ledgerStateAfterEBB)
      [ValidatedColeTx exampleGenTx]
      (fakeMkIsLeader leaderCredentials)
  where
    -- | Normally, we'd have to use 'checkIsLeader' to produce this proof.
    fakeMkIsLeader (ColeLeaderCredentials signKey dlgCert _ _) = PBftIsLeader {
          pbftIsLeaderSignKey = SignKeyColeDSIGN signKey
        , pbftIsLeaderDlgCert = dlgCert
        }

exampleEBB :: ColeBlock
exampleEBB = forgeEBB cfg (SlotNo 0) (BlockNo 0) GenesisHash

exampleSerialisedBlock :: Serialised ColeBlock
exampleSerialisedBlock = Serialised "<BLOCK>"

exampleSerialisedEBB :: Serialised ColeBlock
exampleSerialisedEBB = Serialised "<EBB>"

exampleHeader :: Header ColeBlock
exampleHeader = getHeader exampleBlock

exampleEBBHeader :: Header ColeBlock
exampleEBBHeader = getHeader exampleEBB

exampleSerialisedHeader :: SerialisedHeader ColeBlock
exampleSerialisedHeader = SerialisedHeaderFromDepPair $
    GenDepPair (NestedCtxt (CtxtColeRegular 100)) (Serialised "<HEADER>")

exampleSerialisedEBBHeader :: SerialisedHeader ColeBlock
exampleSerialisedEBBHeader = SerialisedHeaderFromDepPair $
    GenDepPair (NestedCtxt (CtxtColeBoundary 100)) (Serialised "<EBB_HEADER>")

exampleAnnTip :: AnnTip ColeBlock
exampleAnnTip = AnnTip {
      annTipSlotNo  = SlotNo 37
    , annTipBlockNo = BlockNo 23
    , annTipInfo    = TipInfoIsEBB exampleHeaderHash IsNotEBB
    }

exampleChainDepState :: ChainDepState (BlockProtocol ColeBlock)
exampleChainDepState = S.fromList signers
  where
    signers = map (`S.PBftSigner` CC.exampleKeyHash) [1..4]

emptyLedgerState :: LedgerState ColeBlock
emptyLedgerState = ColeLedgerState {
      coleLedgerTipBlockNo = Origin
    , coleLedgerState      = initState
    , coleLedgerTransition = ColeTransitionInfo Map.empty
    }
  where
    initState :: CC.Block.ChainValidationState
    Right initState = runExcept $
      CC.Block.initialChainValidationState ledgerConfig

ledgerStateAfterEBB :: LedgerState ColeBlock
ledgerStateAfterEBB =
      reapplyLedgerBlock ledgerConfig exampleEBB
    . applyChainTick ledgerConfig (SlotNo 0)
    $ emptyLedgerState

exampleLedgerState :: LedgerState ColeBlock
exampleLedgerState =
      reapplyLedgerBlock ledgerConfig exampleBlock
    . applyChainTick ledgerConfig (SlotNo 1)
    $ ledgerStateAfterEBB

exampleHeaderState :: HeaderState ColeBlock
exampleHeaderState = HeaderState (NotOrigin exampleAnnTip) exampleChainDepState

exampleExtLedgerState :: ExtLedgerState ColeBlock
exampleExtLedgerState = ExtLedgerState {
      ledgerState = exampleLedgerState
    , headerState = exampleHeaderState
    }

exampleHeaderHash :: ColeHash
exampleHeaderHash = blockHash exampleBlock

exampleGenTx :: GenTx ColeBlock
exampleGenTx = ColeTx CC.exampleTxId (CC.annotateTxAux CC.exampleTxAux)

exampleGenTxId :: TxId (GenTx ColeBlock)
exampleGenTxId = ColeTxId CC.exampleTxId

exampleUPIState :: CC.UPI.State
exampleUPIState = CC.UPI.initialState ledgerConfig

exampleApplyTxErr :: CC.ApplyMempoolPayloadErr
exampleApplyTxErr =
      CC.MempoolTxErr
    $ CC.UTxOValidationTxValidationError
    $ CC.TxValidationEntropicError "a"
    $ CC.EntropicOverflow 0
