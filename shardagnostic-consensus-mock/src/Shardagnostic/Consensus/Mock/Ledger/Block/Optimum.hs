{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Shardagnostic.Consensus.Mock.Ledger.Block.Optimum (
    SignedSimpleOptimum (..)
  , SimpleOptimumBlock
  , SimpleOptimumExt (..)
  , SimpleOptimumHeader
  , forgeOptimumExt
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise (..))
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Bcc.Binary (FromCBOR (..), ToCBOR (..), serialize')
import           Bcc.Crypto.KES
import           Bcc.Crypto.Util

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Forecast
import           Shardagnostic.Consensus.Ledger.Abstract
import           Shardagnostic.Consensus.Ledger.SupportsProtocol
import           Shardagnostic.Consensus.Mock.Ledger.Address
import           Shardagnostic.Consensus.Mock.Ledger.Block
import           Shardagnostic.Consensus.Mock.Ledger.Forge
import           Shardagnostic.Consensus.Mock.Node.Abstract
import           Shardagnostic.Consensus.Mock.Protocol.Optimum
import           Shardagnostic.Consensus.Protocol.Signed
import           Shardagnostic.Consensus.Util.Condense

import           Shardagnostic.Consensus.Storage.Serialisation

{-------------------------------------------------------------------------------
  Instantiate the @ext@ to suit Optimum
-------------------------------------------------------------------------------}

-- | Simple block extended with the fields required for Optimum
--
-- @c@  is crypto used for the block itself
-- @c'@ is crypto used for the consensus protocol
type SimpleOptimumBlock c c' = SimpleBlock c (SimpleOptimumExt c c')

-- | Header for Proas
type SimpleOptimumHeader c c' = SimpleHeader c (SimpleOptimumExt c c')

-- | Block extension required for Optimum
newtype SimpleOptimumExt c c' = SimpleOptimumExt {
    simpleOptimumExt :: OptimumFields c' (SignedSimpleOptimum c c')
  }
  deriving stock    (Generic, Show, Eq)
  deriving newtype  (Condense)
  deriving anyclass (NoThunks)

-- | Part of the block that gets signed
--
-- TODO: Right now we sign all of the extra Optimum fields. This may or may not
-- be needed. <https://github.com/The-Blockchain-Company/bcc-ledger-specs/issues/530>
-- Of course, this Optimum is merely a proof of concept so it doesn't really
-- matter either way; we include them here primarily to show that we can.
data SignedSimpleOptimum c c' = SignedSimpleOptimum {
      signedSimpleOptimum :: SimpleStdHeader c (SimpleOptimumExt c c')
    , signedOptimumFields :: OptimumExtraFields c'
    }

type instance BlockProtocol (SimpleOptimumBlock c c') = Optimum c'

-- | Sanity check that block and header type synonyms agree
_simpleOptimumHeader :: SimpleOptimumBlock c c' -> SimpleOptimumHeader c c'
_simpleOptimumHeader = simpleHeader

{-------------------------------------------------------------------------------
  Customization of the generic infrastructure
-------------------------------------------------------------------------------}

instance (SimpleCrypto c, Typeable c')
      => MockProtocolSpecific c (SimpleOptimumExt c c') where
  -- | See 'LedgerSupportsProtocol' instance for why we need the 'AddrDist'
  type MockLedgerConfig c (SimpleOptimumExt c c') = AddrDist

{-------------------------------------------------------------------------------
  Evidence that SimpleBlock can support Optimum
-------------------------------------------------------------------------------}

type instance Signed (SimpleOptimumHeader c c') = SignedSimpleOptimum c c'

instance OptimumCrypto c' => SignedHeader (SimpleOptimumHeader c c') where
  headerSigned SimpleHeader{..} = SignedSimpleOptimum {
        signedSimpleOptimum = simpleHeaderStd
      , signedOptimumFields = optimumExtraFields (simpleOptimumExt simpleHeaderExt)
      }

instance ( SimpleCrypto c
         , OptimumCrypto c'
         ) => RunMockBlock c (SimpleOptimumExt c c') where
  mockNetworkMagic = const constructMockNetworkMagic

instance ( SimpleCrypto c
         , OptimumCrypto c'
         , Signable (OptimumKES c') (SignedSimpleOptimum c c')
         ) => BlockSupportsProtocol (SimpleBlock c (SimpleOptimumExt c c')) where
  validateView _ = optimumValidateView (simpleOptimumExt . simpleHeaderExt)

instance ( SimpleCrypto c
         , OptimumCrypto c'
         , Signable (OptimumKES c') (SignedSimpleOptimum c c')
         ) => LedgerSupportsProtocol (SimpleOptimumBlock c c') where
  protocolLedgerView   _ _  = TickedTrivial
  ledgerViewForecastAt _ st = constantForecastOf
                                 TickedTrivial
                                 (getTipSlot st)

{-------------------------------------------------------------------------------
  Forging
-------------------------------------------------------------------------------}

type instance CannotForge (SimpleOptimumBlock c c') = Void

type instance ForgeStateInfo (SimpleOptimumBlock c c') = HotKey c'

type instance ForgeStateUpdateError (SimpleOptimumBlock c c') =
  HotKeyEvolutionError

forgeOptimumExt :: forall c c'.
                 ( SimpleCrypto c
                 , OptimumCrypto c'
                 , Signable (OptimumKES c') (SignedSimpleOptimum c c')
                 )
              => HotKey c'
              -> ForgeExt c (SimpleOptimumExt c c')
forgeOptimumExt hotKey = ForgeExt $ \_cfg isLeader SimpleBlock{..} ->
    let SimpleHeader{..} = simpleHeader

        ext :: SimpleOptimumExt c c'
        ext = SimpleOptimumExt $
          forgeOptimumFields isLeader hotKey $ \optimumExtraFields ->
            SignedSimpleOptimum {
                signedSimpleOptimum = simpleHeaderStd
              , signedOptimumFields = optimumExtraFields
              }
    in SimpleBlock {
        simpleHeader = mkSimpleHeader encode simpleHeaderStd ext
      , simpleBody   = simpleBody
      }

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance OptimumCrypto c' => Serialise (SimpleOptimumExt c c') where
  encode (SimpleOptimumExt OptimumFields{..}) = mconcat [
        encodeSignedKES        optimumSignature
      , encodeOptimumExtraFields optimumExtraFields
      ]
  decode = do
      optimumSignature   <- decodeSignedKES
      optimumExtraFields <- decodeOptimumExtraFields
      return $ SimpleOptimumExt OptimumFields{..}

instance (SimpleCrypto c, OptimumCrypto c')
                        => ToCBOR (SignedSimpleOptimum c c') where
  toCBOR SignedSimpleOptimum{..} = mconcat [
        encode                 signedSimpleOptimum
      , encodeOptimumExtraFields signedOptimumFields
      ]

instance (SimpleCrypto c, OptimumCrypto c')
    => SignableRepresentation (SignedSimpleOptimum c c') where
  getSignableRepresentation = serialize'

encodeOptimumExtraFields :: OptimumCrypto c' => OptimumExtraFields c' -> CBOR.Encoding
encodeOptimumExtraFields OptimumExtraFields{..} = mconcat [
      encode optimumCreator
    , toCBOR optimumRho
    , toCBOR optimumY
    ]

decodeOptimumExtraFields :: forall s c'. OptimumCrypto c'
                       => CBOR.Decoder s (OptimumExtraFields c')
decodeOptimumExtraFields = do
    optimumCreator <- decode
    optimumRho     <- fromCBOR
    optimumY       <- fromCBOR
    return OptimumExtraFields{..}

instance OptimumCrypto c' => EncodeDisk (SimpleOptimumBlock c c') (OptimumChainDepState c')
  -- Default instance

instance OptimumCrypto c' => DecodeDisk (SimpleOptimumBlock c c') (OptimumChainDepState c')
  -- Default instance
