{-# OPTIONS_GHC -Wno-orphans     #-}
module Test.Shardagnostic.Network.Orphans () where

import           Network.TypedProtocol.PingPong.Type (PingPong)
import           Network.TypedProtocol.ReqResp.Type  (ReqResp)

import           Shardagnostic.Network.Util.ShowProxy (ShowProxy (..))


instance ShowProxy PingPong where
    showProxy _ = "PingPong"

instance ShowProxy (ReqResp req resp) where
    showProxy _ = "ReqResp"
