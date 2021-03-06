{-# LANGUAGE DataKinds           #-}

-- | The transaction submission protocol version 2.
--
-- This module ony defines the type of the protocol, and exports all useful
-- functions and types.
--
module Shardagnostic.Network.Protocol.TxSubmission2.Type
  ( TxSubmission2
  , module TxSubmission
  , module Util
  ) where

import           Shardagnostic.Network.Protocol.TxSubmission.Type as TxSubmission
import           Shardagnostic.Network.Protocol.Trans.Hello.Type (Hello)
import           Shardagnostic.Network.Protocol.Trans.Hello.Util as Util

-- | The new version of transaction submission protocol.
--
-- Unlike the original 'TxSubmission' protocol, this protocol starts with
-- agency on the client side, like all other mini-protocols.
--
type TxSubmission2 txid tx = Hello (TxSubmission txid tx) StIdle
