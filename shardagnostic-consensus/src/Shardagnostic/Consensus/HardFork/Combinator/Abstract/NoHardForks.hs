module Shardagnostic.Consensus.HardFork.Combinator.Abstract.NoHardForks (
    NoHardForks (..)
  , noHardForksEpochInfo
  ) where

import           Data.Functor.Identity (runIdentity)

import           Bcc.Slotting.EpochInfo

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Config
import           Shardagnostic.Consensus.HardFork.History as History
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.Protocol.Abstract

import           Shardagnostic.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import           Shardagnostic.Consensus.HardFork.Combinator.PartialConfig

{-------------------------------------------------------------------------------
  Blocks that don't /have/ any transitions
-------------------------------------------------------------------------------}

class SingleEraBlock blk => NoHardForks blk where
  -- | Extract 'EraParams' from the top-level config
  --
  -- The HFC itself does not care about this, as it must be given the full shape
  -- across /all/ eras.
  getEraParams :: TopLevelConfig blk -> EraParams

  -- | Construct partial consensus config from full consensus config
  --
  -- NOTE: This is basically just losing 'EpochInfo', but that is constant
  -- anyway when we are dealing with a single era.
  toPartialConsensusConfig :: proxy blk
                           -> ConsensusConfig (BlockProtocol blk)
                           -> PartialConsensusConfig (BlockProtocol blk)

  -- | Construct partial ledger config from full ledger config
  --
  -- See also 'toPartialConsensusConfig'
  toPartialLedgerConfig :: proxy blk
                        -> LedgerConfig blk -> PartialLedgerConfig blk

noHardForksEpochInfo :: (Monad m, NoHardForks blk)
                     => TopLevelConfig blk
                     -> EpochInfo m
noHardForksEpochInfo cfg =
      hoistEpochInfo (pure . runIdentity)
    $ fixedEpochInfo
        (History.eraEpochSize  params)
        (History.eraSlotLength params)
  where
    params :: EraParams
    params = getEraParams cfg
