{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shardagnostic.Network.Protocol.LocalTxSubmission.Direct (
    direct
  ) where

import           Shardagnostic.Network.Protocol.LocalTxSubmission.Client
import           Shardagnostic.Network.Protocol.LocalTxSubmission.Server


direct :: forall tx reject m a b.
          Monad m
       => LocalTxSubmissionClient tx reject m a
       -> LocalTxSubmissionServer tx reject m b
       -> m (a, b)
direct (LocalTxSubmissionClient mclient) server =
    mclient >>= \client -> directSender client server
  where
    directSender :: LocalTxClientStIdle     tx reject m a
                 -> LocalTxSubmissionServer tx reject m b
                 -> m (a, b)
    directSender (SendMsgSubmitTx tx k) LocalTxSubmissionServer{recvMsgSubmitTx} = do
        (res, server') <- recvMsgSubmitTx tx
        client' <- k res
        directSender client' server'

    directSender (SendMsgDone a) LocalTxSubmissionServer{recvMsgDone = b} =
        return (a,b)

