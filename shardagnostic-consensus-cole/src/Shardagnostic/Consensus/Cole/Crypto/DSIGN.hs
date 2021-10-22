{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Cole digital signatures.
module Shardagnostic.Consensus.Cole.Crypto.DSIGN (
    ColeDSIGN
  , HasSignTag (..)
  , SigDSIGN (..)
  , SignKeyDSIGN (..)
  , VerKeyDSIGN (..)
  ) where


import           Control.Exception (throw)
import           Data.ByteString (ByteString)
import           Data.Coerce (coerce)
import           Data.Proxy (Proxy (..))
import           GHC.Generics (Generic)
import           NoThunks.Class (InspectHeapNamed (..), NoThunks)

import           Bcc.Binary
import qualified Bcc.Chain.Block as CC.Block
import qualified Bcc.Chain.UTxO as CC.UTxO
import           Bcc.Crypto (ProtocolMagicId, SignTag (..), Signature (..),
                     SigningKey (..), VerificationKey (..), deterministicKeyGen,
                     signRaw, toVerification, verifySignatureRaw)
import           Bcc.Crypto.DSIGN.Class
import           Bcc.Crypto.Seed (SeedBytesExhausted (..), getBytesFromSeed)
import qualified Bcc.Crypto.Signing as Crypto
import qualified Bcc.Crypto.Wallet as CC

import           Shardagnostic.Consensus.Util (eitherToMaybe)
import           Shardagnostic.Consensus.Util.Condense

class (HasSignTag a, Decoded a) => ColeSignable a
instance (HasSignTag a, Decoded a) => ColeSignable a

class HasSignTag a where
  signTag :: VerKeyDSIGN ColeDSIGN -> proxy a -> SignTag

signTagFor :: forall a. HasSignTag a
           => VerKeyDSIGN ColeDSIGN -> a -> SignTag
signTagFor genKey _ = signTag genKey (Proxy @a)

instance HasSignTag CC.UTxO.TxSigData where
  signTag _ = const SignTx

instance HasSignTag (Annotated CC.Block.ToSign ByteString) where
  signTag (VerKeyColeDSIGN vk) = const $ SignBlock vk

data ColeDSIGN

instance DSIGNAlgorithm ColeDSIGN where

    type SeedSizeDSIGN    ColeDSIGN = 32
    type SizeVerKeyDSIGN  ColeDSIGN = 64
    type SizeSignKeyDSIGN ColeDSIGN = 128
    type SizeSigDSIGN     ColeDSIGN = 64

    algorithmNameDSIGN _ = "ColeDSIGN"

    -- Context required for Cole digital signatures
    --
    -- We require the the protocol magic as well as the verification key of the
    -- genesis stakeholder of which the signing node is a delegate, which is
    -- required for signing blocks.
    type ContextDSIGN ColeDSIGN = (ProtocolMagicId, VerKeyDSIGN ColeDSIGN)

    newtype VerKeyDSIGN ColeDSIGN = VerKeyColeDSIGN VerificationKey
        deriving (Show, Eq, Generic)
        deriving NoThunks via InspectHeapNamed "VerKeyDSIGN ColeDSIGN" (VerKeyDSIGN ColeDSIGN)

    newtype SignKeyDSIGN ColeDSIGN = SignKeyColeDSIGN SigningKey
        deriving (Show, Generic)
        deriving NoThunks via InspectHeapNamed "SignKeyDSIGN ColeDSIGN" (SignKeyDSIGN ColeDSIGN)

    newtype SigDSIGN ColeDSIGN = SigColeDSIGN (Signature CC.Block.ToSign)
        deriving (Show, Eq, Generic)
        deriving NoThunks via InspectHeapNamed "SigDSIGN ColeDSIGN" (SigDSIGN ColeDSIGN)

    type Signable ColeDSIGN = ColeSignable

    genKeyDSIGN seed =
        SignKeyColeDSIGN . snd $ deterministicKeyGen seedBytes
      where
        seedBytes = case getBytesFromSeed 32 seed of
          Just (x,_) -> x
          Nothing    -> throw $ SeedBytesExhausted (-1) -- TODO We can't get the seed size!

    deriveVerKeyDSIGN (SignKeyColeDSIGN sk) = VerKeyColeDSIGN $ toVerification sk

    signDSIGN (magic, genKey) a (SignKeyColeDSIGN sk) =
        SigColeDSIGN
        . coerce
        $ signRaw magic (Just $ signTagFor genKey a) sk (recoverBytes a)

    verifyDSIGN (magic, genKey) (VerKeyColeDSIGN vk) a (SigColeDSIGN sig) =
        if verifySignatureRaw vk (Crypto.signTag magic (signTagFor genKey a) <> recoverBytes a) $ coerce sig
          then Right ()
          else Left "Verification failed"

    rawSerialiseVerKeyDSIGN (VerKeyColeDSIGN (VerificationKey vk)) = CC.unXPub vk
    rawSerialiseSignKeyDSIGN (SignKeyColeDSIGN (SigningKey sk)) = CC.unXPrv sk
    rawSerialiseSigDSIGN (SigColeDSIGN (Signature sig)) = CC.unXSignature sig

    rawDeserialiseVerKeyDSIGN bs =
      VerKeyColeDSIGN . VerificationKey <$> (eitherToMaybe $ CC.xpub bs)
    rawDeserialiseSignKeyDSIGN bs =
      SignKeyColeDSIGN . SigningKey <$> (eitherToMaybe $ CC.xprv bs)
    rawDeserialiseSigDSIGN bs =
      SigColeDSIGN . Signature <$> (eitherToMaybe $ CC.xsignature bs)

instance Condense (SigDSIGN ColeDSIGN) where
    condense (SigColeDSIGN s) = show s
