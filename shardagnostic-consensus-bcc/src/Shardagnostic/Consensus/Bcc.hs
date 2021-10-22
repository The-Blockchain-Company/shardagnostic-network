{-# LANGUAGE DataKinds #-}

module Shardagnostic.Consensus.Bcc (
    -- * The block type of the Bcc block chain
    BccBlock
    -- * Supported protocols
  , ProtocolCole
  , ProtocolBcc
  , ProtocolSophie
    -- * Abstract over the various protocols
  , ProtocolParamsEvie (..)
  , ProtocolParamsAurum (..)
  , ProtocolParamsCole (..)
  , ProtocolParamsJen (..)
  , ProtocolParamsSophie (..)
  , ProtocolTransitionParamsSophieBased (..)
  , module X
  ) where

import           Shardagnostic.Consensus.HardFork.Combinator

import           Shardagnostic.Consensus.Cole.Ledger
import           Shardagnostic.Consensus.Cole.Node as X

import           Shardagnostic.Consensus.Sophie.Ledger
import           Shardagnostic.Consensus.Sophie.Node as X
import           Shardagnostic.Consensus.Sophie.SophieHFC

import           Shardagnostic.Consensus.Bcc.Block
import           Shardagnostic.Consensus.Bcc.Node

{-------------------------------------------------------------------------------
  Supported protocols

  We list these as explicit definitions here (rather than derived through
  'BlockProtocol'), and then /verify/ in 'verifyProtocol' that these definitions
  match. This provides an additional sanity check that we are not accidentally
  breaking any assumptions made in @bcc-node@.
-------------------------------------------------------------------------------}

type ProtocolCole   = HardForkProtocol '[ ColeBlock ]
type ProtocolBcc = HardForkProtocol '[ ColeBlock
                                         , SophieBlock StandardSophie
                                         , SophieBlock StandardEvie
                                         , SophieBlock StandardJen
                                         , SophieBlock StandardAurum
                                         ]
