{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Util.Orphans.SignableRepresentation () where

import           Bcc.Crypto.Util

instance SignableRepresentation () where
  getSignableRepresentation () = ""
