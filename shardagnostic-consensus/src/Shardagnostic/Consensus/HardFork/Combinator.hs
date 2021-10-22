{-# LANGUAGE PatternSynonyms #-}

-- | The hard fork combinator
--
-- Intended for unqualified import
module Shardagnostic.Consensus.HardFork.Combinator (module X) where

-- Defines 'SingleEraInfo' and 'LedgerEraInfo'
import           Shardagnostic.Consensus.HardFork.Combinator.Info as X

-- Defines 'SingleEraBlock' and 'CanHardFork'
import           Shardagnostic.Consensus.HardFork.Combinator.Abstract as X

-- Defines 'HardForkProtocol', 'HardForkBlock', and 'LedgerState',
-- as well as various config types.
import           Shardagnostic.Consensus.HardFork.Combinator.Basics as X

-- Defines 'HasPartialConsensusConfig' and 'HasPartialLedgerConfig'
import           Shardagnostic.Consensus.HardFork.Combinator.PartialConfig as X

-- Instances for 'BlockHasCodecConfig', 'GetHeader', 'HasHeader',
-- 'BasicEnvelopeValidation'
import           Shardagnostic.Consensus.HardFork.Combinator.Block as X

-- Instances for 'IsLedger', 'ApplyBlock', 'UpdateLedger',
-- 'LedgerSupportsProtocol', 'HasHardForkHistory', 'ValidateEnvelope'
import           Shardagnostic.Consensus.HardFork.Combinator.Ledger as X

-- Instances for 'ApplyTx', 'HasTxId', 'HasTxs'
import           Shardagnostic.Consensus.HardFork.Combinator.Mempool as X

-- Instance for 'ConsensusProtocol'
import           Shardagnostic.Consensus.HardFork.Combinator.Protocol as X

-- Instance for 'CommonProtocolParams'
import           Shardagnostic.Consensus.HardFork.Combinator.Ledger.CommonProtocolParams as X ()

-- Instance for 'LedgerSupportsPeerSelection'
import           Shardagnostic.Consensus.HardFork.Combinator.Ledger.PeerSelection as X ()

-- Instances for 'ShowQuery' and 'QueryLedger'
-- Definition of 'Query', required for serialisation code
import           Shardagnostic.Consensus.HardFork.Combinator.Ledger.Query as X

-- Instance for 'ChainSelection'
import           Shardagnostic.Consensus.HardFork.Combinator.Protocol.ChainSel as X

-- Re-export only the types that are useful in RunNode instances for HFC blocks
import           Shardagnostic.Consensus.HardFork.Combinator.AcrossEras as X
                     (MismatchEraInfo (..), OneEraApplyTxErr (..),
                     OneEraBlock (..), OneEraGenTx (..), OneEraGenTxId (..),
                     OneEraHash (..), OneEraHeader (..), OneEraTipInfo (..),
                     PerEraCodecConfig (..), PerEraLedgerConfig (..))

-- Re-export types required to initialize 'ProtocolInfo'
import           Shardagnostic.Consensus.HardFork.Combinator.AcrossEras as X
                     (PerEraBlockConfig (..), PerEraConsensusConfig (..),
                     PerEraStorageConfig (..))

-- Defines the various translation types required for concrete HFC instances
import           Shardagnostic.Consensus.HardFork.Combinator.Translation as X

-- Combinator for 'BlockForging'
import           Shardagnostic.Consensus.HardFork.Combinator.Forging as X
                     (HardForkForgeStateInfo (..), hardForkBlockForging)

-- Instances for 'RunNode' and 'ConfigSupportsNode'
import           Shardagnostic.Consensus.HardFork.Combinator.Node as X ()

-- Instance for 'NodeInitStorage'
import           Shardagnostic.Consensus.HardFork.Combinator.Node.InitStorage as X ()

-- Instance for 'BlockSupportsMetrics'
import           Shardagnostic.Consensus.HardFork.Combinator.Node.Metrics as X ()

-- Definition of InPairs (required to define translations)
import           Shardagnostic.Consensus.HardFork.Combinator.Util.InPairs as X
                     (InPairs (..))

-- Definition of Telescope (required to define serialisation code)
import           Shardagnostic.Consensus.HardFork.Combinator.Util.Telescope as X
                     (Telescope (..))

-- Definition of 'Mismatch' (required to define serialisation code)
import           Shardagnostic.Consensus.HardFork.Combinator.Util.Match as X
                     (Mismatch (..))

-- Definition of HardForkState (required to define serialisation code)
-- Also export functions required to define 'protocolInfo'.
import           Shardagnostic.Consensus.HardFork.Combinator.State as X
                     (HardForkState (..), initHardForkState)

-- Definition of 'InjectTx' (required to define tx injections)
import           Shardagnostic.Consensus.HardFork.Combinator.InjectTxs as X
                     (InjectTx, InjectValidatedTx, cannotInjectTx,
                     cannotInjectValidatedTx, pattern InjectTx,
                     pattern InjectValidatedTx)
import           Shardagnostic.Consensus.HardFork.Combinator.Util.Functors as X
                     (Product2 (..))

-- Omitted from this export:
--
-- * "Shardagnostic.Consensus.HardFork.Combinator.State"
--   This defines 'HardForkState', a wrapper around a 'Telescope'. We use this
--   to define 'HardForkLedgerState', 'HardForkLedgerView' as well as
--   'HardForkChainDepState', but the type itself should mostly be internal to
--   the hard fork combinator. We do export the constructor for it, as this may
--   be required for serialisation code.
--
-- * "module Shardagnostic.Consensus.HardFork.Combinator.State.Infra"
--   This module is only separate from @.State@ to avoid some cyclic module
--   dependencies. Most modules internally to the HFC should import from
--   @.State@ directly, and outside of the HFC not even @.State@ should be
--   needed (see above).
--
-- * "Shardagnostic.Consensus.HardFork.Combinator.Protocol.LedgerView"
--   This is internal to "Shardagnostic.Consensus.HardFork.Combinator.Protocol"
--
-- * "Shardagnostic.Consensus.HardFork.Combinator.Protocol.State"
--   This is internal to "Shardagnostic.Consensus.HardFork.Combinator.Protocol"
--
-- * "Shardagnostic.Consensus.HardFork.Combinator.Degenerate"
--   This defines 'DegenFork', which is useful as a test case that the hard
--   fork combinator when applied to a single block results in a system
--   that is equivalent to just using that single block directly.
--
-- * "Shardagnostic.Consensus.HardFork.Combinator.Embed.Unary"
--   Mostly used in combination with 'DegenFork'.
--
-- * "Shardagnostic.Consensus.HardFork.Combinator.Embed.Nary"
--   Used for injection into n-ary sums. Alternative to @Unary@.
--
-- * Most of @Shardagnostic.Consensus.HardFork.Combinator.SingleEra.*@
--   These types are primarily used internally to define the HFC types.
--   In a few cases some of the HFC types /are/ types from the SingleEra
--   module hierarchy directly; in such cases, we should export them from
--   this module.
--   TODO: Currently we only do this for @SingleEra.Info@, but we might also
--   need to do it for other types.
--
-- * Shardagnostic.Consensus.HardFork.Combinator.Util.*
--   We omit most utility functions and types, which are for internal use. Some
--   exceptions the defintion of InPairs, which will be required to define
--   translations, and the definition of a Telescope, which might be needed to
--   define serialisation code.
