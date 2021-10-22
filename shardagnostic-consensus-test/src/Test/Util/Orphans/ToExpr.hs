{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Util.Orphans.ToExpr () where

import           Data.TreeDiff (ToExpr (..))

import           Bcc.Slotting.Slot

import           Shardagnostic.Network.Block
import           Shardagnostic.Network.Point

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.HeaderValidation
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.Ledger.Extended
import           Shardagnostic.Consensus.Protocol.Abstract

{-------------------------------------------------------------------------------
  shardagnostic-network
-------------------------------------------------------------------------------}

instance ToExpr SlotNo
instance ToExpr BlockNo

instance ToExpr t => ToExpr (WithOrigin t)
instance ToExpr (HeaderHash blk) => ToExpr (Point blk)
instance ToExpr (HeaderHash blk) => ToExpr (RealPoint blk)
instance (ToExpr slot, ToExpr hash) => ToExpr (Block slot hash)

{-------------------------------------------------------------------------------
  shardagnostic-consensus
-------------------------------------------------------------------------------}

instance ( ToExpr (LedgerState blk)
         , ToExpr (ChainDepState (BlockProtocol blk))
         , ToExpr (TipInfo blk)
         ) => ToExpr (ExtLedgerState blk)

instance ( ToExpr (ChainDepState (BlockProtocol blk))
         , ToExpr (TipInfo blk)
         ) => ToExpr (HeaderState blk)

instance ( ToExpr (TipInfo blk)
         ) => ToExpr (AnnTip blk)
