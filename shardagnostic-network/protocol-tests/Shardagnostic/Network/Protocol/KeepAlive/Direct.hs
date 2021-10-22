{-# LANGUAGE NamedFieldPuns #-}

module Shardagnostic.Network.Protocol.KeepAlive.Direct where

import Shardagnostic.Network.Protocol.KeepAlive.Client
import Shardagnostic.Network.Protocol.KeepAlive.Server

direct :: Monad m
       => KeepAliveServer m a
       -> KeepAliveClient m b
       -> m (a, b)
direct KeepAliveServer { recvMsgDone }
       (SendMsgDone mdone) =
    (,) <$> recvMsgDone <*> mdone
direct KeepAliveServer { recvMsgKeepAlive }
       (SendMsgKeepAlive _cookie mclient) = do
    server <- recvMsgKeepAlive
    client <- mclient
    direct server client
