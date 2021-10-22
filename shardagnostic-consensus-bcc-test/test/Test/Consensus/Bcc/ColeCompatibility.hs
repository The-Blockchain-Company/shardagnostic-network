{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | The purpose of these tests are to ensure that when in the Cole era, nodes
--  using @BccBlock@ can still communicate with older nodes using
--  @ColeBlock@. This is tested by running roundtrip tests using the encoders
--  of @ColeBlock@ and the decoders of @BccBlock@ and vice versa. By
--  introducing a newtype wrapper for each direction, we are able to reuse the
--  existing roundtrip test functions.
module Test.Consensus.Bcc.ColeCompatibility (tests) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Coerce (Coercible, coerce)
import           Data.SOP.BasicFunctors

import qualified Bcc.Chain.Cole.API as CC

import           Shardagnostic.Network.Block (Serialised (..))

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Ledger.Query
import           Shardagnostic.Consensus.Ledger.SupportsMempool
import           Shardagnostic.Consensus.Node.NetworkProtocolVersion
import           Shardagnostic.Consensus.Node.Run
import           Shardagnostic.Consensus.Node.Serialisation
import           Shardagnostic.Consensus.Storage.Serialisation
import           Shardagnostic.Consensus.TypeFamilyWrappers

import           Shardagnostic.Consensus.HardFork.Combinator (NestedCtxt_ (..))

import           Shardagnostic.Consensus.Cole.Ledger
import           Shardagnostic.Consensus.Cole.Node ()

import           Shardagnostic.Consensus.Bcc.Block
import           Shardagnostic.Consensus.Bcc.Node
import           Shardagnostic.Consensus.Sophie.Ledger.Config (CodecConfig (..))

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Roundtrip

import           Test.Consensus.Cole.Generators ()

import           Test.Consensus.Bcc.Generators (epochSlots)
import           Test.Consensus.Bcc.MockCrypto (MockCryptoCompatCole)

tests :: TestTree
tests = adjustOption reduceTests $
    testGroup "Cole compatibility" [
        testGroup "Cole to Bcc" [
              testProperty "roundtrip block" $
                roundtrip' @ColeToBcc
                  (encodeDisk coleToBccCodeConfig)
                  (decodeDisk coleToBccCodeConfig)
            , testGroup "SerialiseNodeToNode" $
                roundtrip_SerialiseNodeToNode   coleToBccCodeConfig
            , testGroup "SerialiseNodeToClient" $
                roundtrip_SerialiseNodeToClient coleToBccCodeConfig
            ]
      , testGroup "Bcc to Cole" [
              testProperty "roundtrip block" $
                roundtrip' @BccToCole
                  (encodeDisk bccToColeCodeConfig)
                  (decodeDisk bccToColeCodeConfig)
            , testGroup "SerialiseNodeToNode" $
                roundtrip_SerialiseNodeToNode   bccToColeCodeConfig
            , testGroup "SerialiseNodeToClient" $
                roundtrip_SerialiseNodeToClient bccToColeCodeConfig
            ]
      ]
  where
    -- | We're not trying to find edge cases in the roundtrip tests, we just
    -- want to check compatibility. In case of incompatibility, the first test
    -- will probably fail already.
    reduceTests (QuickCheckTests n) = QuickCheckTests (1 `max` (div n 10))

coleCodecConfig :: CodecConfig ColeBlock
coleCodecConfig = ColeCodecConfig epochSlots

coleToBccCodeConfig :: CodecConfig ColeToBcc
coleToBccCodeConfig = CodecConfigB2C coleCodecConfig

bccToColeCodeConfig :: CodecConfig BccToCole
bccToColeCodeConfig = CodecConfigC2B coleCodecConfig

{------------------------------------------------------------------------------
  Common setup
------------------------------------------------------------------------------}

-- | We don't use Sophie at all in this module, so we just pick some crypto
-- and use that everywhere.
type Crypto = MockCryptoCompatCole

coleNodeToNodeVersion :: BlockNodeToNodeVersion ColeBlock
coleNodeToNodeVersion = ColeNodeToNodeVersion1

coleNodeToClientVersion :: BlockNodeToClientVersion ColeBlock
coleNodeToClientVersion = ColeNodeToClientVersion1

bccNodeToNodeVersion :: BlockNodeToNodeVersion (BccBlock Crypto)
bccNodeToNodeVersion = BccNodeToNodeVersion1

bccNodeToClientVersion :: BlockNodeToClientVersion (BccBlock Crypto)
bccNodeToClientVersion = BccNodeToClientVersion1

pb :: Proxy ColeBlock
pb = Proxy

toBccCodecConfig ::
     CodecConfig ColeBlock
  -> CodecConfig (BccBlock Crypto)
toBccCodecConfig codecConfigCole =
    BccCodecConfig
      codecConfigCole
      SophieCodecConfig
      SophieCodecConfig
      SophieCodecConfig
      SophieCodecConfig

{------------------------------------------------------------------------------
  Cole to Bcc
------------------------------------------------------------------------------}

-- | Encoded Cole values can be decoded as Bcc values in the following
-- cases:
--
-- * The @HardForkNodeTo(Node|Client)Disabled@ version is used
-- * Blocks and headers stored on disk
--
-- Note that ledger state and all other types stored as part of the ledger
-- snapshot are __not__ forwards compatible.
newtype ColeToBcc                       = B2C        { unB2C        ::            ColeBlock   } deriving (Eq, Show)
newtype instance Header ColeToBcc       = HeaderB2C  { unHeaderB2C  :: Header     ColeBlock   } deriving (Eq, Show)
newtype instance GenTx ColeToBcc        = GenTxB2C   { unGenTxB2C   :: GenTx      ColeBlock   } deriving (Eq, Show)
newtype instance TxId (GenTx ColeToBcc) = GenTxIdB2C { unGenTxIdB2C :: GenTxId    ColeBlock   } deriving (Eq, Show)
newtype instance BlockQuery ColeToBcc a = QueryB2C   { unQueryB2C   :: BlockQuery ColeBlock a } deriving (Eq, Show)

newtype instance NestedCtxt_ ColeToBcc f a where
  NestedCtxt_B2C :: NestedCtxt_ ColeBlock     f a
                 -> NestedCtxt_ ColeToBcc f a

deriving instance Show (NestedCtxt_ ColeToBcc Header a)

unNestedCtxt_B2C :: NestedCtxt_ ColeToBcc f a -> NestedCtxt_ ColeBlock f a
unNestedCtxt_B2C (NestedCtxt_B2C ctxt) = ctxt

type instance HeaderHash ColeToBcc = HeaderHash ColeBlock
type instance ApplyTxErr ColeToBcc = ApplyTxErr ColeBlock

instance HasNetworkProtocolVersion ColeToBcc

instance ConvertRawHash ColeToBcc where
  toShortRawHash   _ = toShortRawHash   pb
  fromShortRawHash _ = fromShortRawHash pb
  hashSize         _ = hashSize         pb

data instance CodecConfig ColeToBcc = CodecConfigB2C (CodecConfig ColeBlock)

instance SameDepIndex (NestedCtxt_ ColeToBcc Header) where
  sameDepIndex (NestedCtxt_B2C ctxt1) (NestedCtxt_B2C ctxt2) =
      sameDepIndex ctxt1 ctxt2

instance HasNestedContent Header ColeToBcc where
  unnest hdr = case unnest (unHeaderB2C hdr) of
      DepPair ctxt a -> DepPair (mapNestedCtxt NestedCtxt_B2C ctxt) a
  nest (DepPair ctxt a) =
      HeaderB2C $ nest (DepPair (mapNestedCtxt unNestedCtxt_B2C ctxt) a)

instance ShowQuery (BlockQuery ColeToBcc) where
  showResult (QueryB2C query) = showResult query

instance SameDepIndex (BlockQuery ColeToBcc) where
  sameDepIndex (QueryB2C q1) (QueryB2C q2) = sameDepIndex q1 q2

{------------------------------------------------------------------------------
  Cole to Bcc: Disk
------------------------------------------------------------------------------}

encodeDiskB2C ::
     forall f cole b2c.
     ( EncodeDisk ColeBlock (f ColeBlock)
     , Coercible cole (f ColeBlock)
     )
  => Proxy f
  -> (b2c -> cole)
  -> CodecConfig ColeToBcc
  -> b2c
  -> Encoding
encodeDiskB2C _ toCole (CodecConfigB2C ccfg) x =
    encodeDisk ccfg (toCole' x)
  where
    toCole' :: b2c -> f ColeBlock
    toCole' = coerce . toCole

decodeDiskB2C ::
     forall f bcc b2c.
     ( DecodeDisk (BccBlock Crypto) (f (BccBlock Crypto))
     , Coercible bcc (f (BccBlock Crypto))
     )
  => Proxy f
  -> (bcc -> b2c)
  -> CodecConfig ColeToBcc
  -> forall s. Decoder s b2c
decodeDiskB2C _ fromBcc (CodecConfigB2C ccfg) =
    fromBcc' <$> decodeDisk (toBccCodecConfig ccfg)
  where
    fromBcc' :: f (BccBlock Crypto) -> b2c
    fromBcc' = fromBcc . coerce

instance EncodeDisk ColeToBcc ColeToBcc where
  encodeDisk = encodeDiskB2C (Proxy @I) unB2C

instance DecodeDisk ColeToBcc (Lazy.ByteString -> ColeToBcc) where
  decodeDisk = decodeDiskB2C
                 (Proxy @((->) Lazy.ByteString))
                 (fmap (\(BlockCole blk) -> B2C blk))

instance EncodeDiskDep (NestedCtxt Header) ColeToBcc where
  encodeDiskDep (CodecConfigB2C ccfg) =
      encodeDiskDep ccfg . mapNestedCtxt unNestedCtxt_B2C

instance DecodeDiskDep (NestedCtxt Header) ColeToBcc where
  decodeDiskDep (CodecConfigB2C ccfg) =
      decodeDiskDep (toBccCodecConfig ccfg) . mapNestedCtxt (NCZ . unNestedCtxt_B2C)

{------------------------------------------------------------------------------
  Cole to Bcc: NodeToNode
------------------------------------------------------------------------------}

encodeNodeToNodeB2C ::
     forall f cole b2c.
     ( SerialiseNodeToNode ColeBlock (f ColeBlock)
     , Coercible cole (f ColeBlock)
     )
  => Proxy f
  -> (b2c -> cole)
  -> CodecConfig ColeToBcc
  -> BlockNodeToNodeVersion ColeToBcc
  -> b2c
  -> Encoding
encodeNodeToNodeB2C _ toCole (CodecConfigB2C ccfg) () x =
    encodeNodeToNode ccfg coleNodeToNodeVersion (toCole' x)
  where
    toCole' :: b2c -> f ColeBlock
    toCole' = coerce . toCole

decodeNodeToNodeB2C ::
     forall f bcc b2c.
     ( SerialiseNodeToNode (BccBlock Crypto) (f (BccBlock Crypto))
     , Coercible bcc (f (BccBlock Crypto))
     )
  => Proxy f
  -> (bcc -> b2c)
  -> CodecConfig ColeToBcc
  -> BlockNodeToNodeVersion ColeToBcc
  -> forall s. Decoder s b2c
decodeNodeToNodeB2C _ fromBcc (CodecConfigB2C ccfg) () =
    fromBcc' <$>
      decodeNodeToNode (toBccCodecConfig ccfg) bccNodeToNodeVersion
  where
    fromBcc' :: f (BccBlock Crypto) -> b2c
    fromBcc' = fromBcc . coerce

instance SerialiseNodeToNode ColeToBcc ColeToBcc where
  encodeNodeToNode = encodeNodeToNodeB2C (Proxy @I) unB2C
  decodeNodeToNode = decodeNodeToNodeB2C (Proxy @I) (\(BlockCole blk) -> B2C blk)

instance SerialiseNodeToNode ColeToBcc (Serialised ColeToBcc) where
  encodeNodeToNode = encodeNodeToNodeB2C (Proxy @Serialised) id
  decodeNodeToNode = decodeNodeToNodeB2C (Proxy @Serialised) id

instance SerialiseNodeToNode ColeToBcc (SerialisedHeader ColeToBcc) where
  encodeNodeToNode = encodeNodeToNodeB2C
                       (Proxy @SerialisedHeader)
                       (castSerialisedHeader unNestedCtxt_B2C)
  decodeNodeToNode = decodeNodeToNodeB2C
                       (Proxy @SerialisedHeader)
                       (castSerialisedHeader (\(NCZ ctxt) -> NestedCtxt_B2C ctxt))

instance SerialiseNodeToNode ColeToBcc (Header ColeToBcc) where
  encodeNodeToNode = encodeNodeToNodeB2C (Proxy @Header) unHeaderB2C
  decodeNodeToNode = decodeNodeToNodeB2C (Proxy @Header) (\(HeaderCole hdr) -> HeaderB2C hdr)

instance SerialiseNodeToNode ColeToBcc (GenTx ColeToBcc) where
  encodeNodeToNode = encodeNodeToNodeB2C (Proxy @GenTx) unGenTxB2C
  decodeNodeToNode = decodeNodeToNodeB2C (Proxy @GenTx) (\(GenTxCole tx) -> GenTxB2C tx)

instance SerialiseNodeToNode ColeToBcc (GenTxId ColeToBcc) where
  encodeNodeToNode = encodeNodeToNodeB2C (Proxy @WrapGenTxId) unGenTxIdB2C
  decodeNodeToNode = decodeNodeToNodeB2C (Proxy @WrapGenTxId) (\(GenTxIdCole txid) -> GenTxIdB2C txid)

instance SerialiseNodeToNodeConstraints ColeToBcc where
  estimateBlockSize = estimateBlockSize . unHeaderB2C

{------------------------------------------------------------------------------
  Cole to Bcc: NodeToClient
------------------------------------------------------------------------------}

-- | We want to encode cole-to-bcc compatibility types using cole
-- serializations. With that in mind, this is a helper function for implementing
-- @encodeNodeToClient@ for cole-to-bcc compatibility type: @b2c@. This
-- works by projecting to the cole type and encoding that.
encodeNodeToClientB2C ::
     forall f cole b2c.
     ( SerialiseNodeToClient ColeBlock (f ColeBlock)
     , Coercible cole (f ColeBlock)
     )
  => Proxy f
  -- ^ @f@ is an intermediate type, used for its @SerialiseNodeToClient@
  -- instance. @f@ is usually a newtype that wraps a @cole@ value.
  -> (b2c -> cole)
  -- ^ Convert (usually a simple projection) from the cole-to-bcc
  -- compatibility type to the cole type.
  -> CodecConfig ColeToBcc
  -> BlockNodeToClientVersion ColeToBcc
  -> b2c
  -- ^ The value to encode
  -> Encoding
encodeNodeToClientB2C _ toCole (CodecConfigB2C ccfg) () x =
    encodeNodeToClient @ColeBlock ccfg coleNodeToClientVersion (toCole' x)
  where
    toCole' :: b2c -> f ColeBlock
    toCole' = coerce . toCole

-- | We want to decode cole serializations into cole-to-bcc compatibility
-- types. With that in mind, this is a helper function for implementing
-- @decodeNodeToClient@ for cole-to-bcc compatibility type: @b2c@. This
-- works by decoding as the cole type and wrapping that in the cole-to-bcc
-- compatibility type.
decodeNodeToClientB2C ::
     forall f bcc b2c.
     ( SerialiseNodeToClient (BccBlock Crypto) (f (BccBlock Crypto))
     , Coercible bcc (f (BccBlock Crypto))
     )
  => Proxy f
  -- ^ @f@ is an intermediate type, used for its @SerialiseNodeToClient@
  -- instance. @f@ is usually a newtype that wraps a @cole@ value.
  -> (bcc -> b2c)
  -> CodecConfig ColeToBcc
  -> BlockNodeToClientVersion ColeToBcc
  -> forall s. Decoder s b2c
decodeNodeToClientB2C _ fromBcc (CodecConfigB2C ccfg) () =
    fromBcc' <$>
      decodeNodeToClient
        @(BccBlock Crypto)
        (toBccCodecConfig ccfg)
        bccNodeToClientVersion
  where
    fromBcc' :: f (BccBlock Crypto) -> b2c
    fromBcc' = fromBcc . coerce

instance SerialiseNodeToClient ColeToBcc ColeToBcc where
  encodeNodeToClient = encodeNodeToClientB2C (Proxy @I) unB2C
  decodeNodeToClient = decodeNodeToClientB2C (Proxy @I) (\(BlockCole blk) -> B2C blk)

instance SerialiseNodeToClient ColeToBcc (Serialised ColeToBcc) where
  encodeNodeToClient = encodeNodeToClientB2C (Proxy @Serialised) id
  decodeNodeToClient = decodeNodeToClientB2C (Proxy @Serialised) id

instance SerialiseNodeToClient ColeToBcc (GenTx ColeToBcc) where
  encodeNodeToClient = encodeNodeToClientB2C (Proxy @GenTx) unGenTxB2C
  decodeNodeToClient = decodeNodeToClientB2C (Proxy @GenTx) (\(GenTxCole tx) -> GenTxB2C tx)

-- | @'ApplyTxErr' 'ColeToBcc'@
instance SerialiseNodeToClient ColeToBcc CC.ApplyMempoolPayloadErr where
  encodeNodeToClient = encodeNodeToClientB2C (Proxy @WrapApplyTxErr) id
  decodeNodeToClient = decodeNodeToClientB2C (Proxy @WrapApplyTxErr) (\(ApplyTxErrCole err) -> err)

instance SerialiseNodeToClient ColeToBcc (SomeSecond BlockQuery ColeToBcc) where
  encodeNodeToClient = encodeNodeToClientB2C
                         (Proxy @(SomeSecond BlockQuery))
                         (\(SomeSecond q) -> SomeSecond (unQueryB2C q))
  decodeNodeToClient = decodeNodeToClientB2C
                         (Proxy @(SomeSecond BlockQuery))
                         (\(SomeSecond (QueryIfCurrentCole q)) -> SomeSecond (QueryB2C q))

instance SerialiseResult ColeToBcc (BlockQuery ColeToBcc) where
  encodeResult (CodecConfigB2C ccfg) () (QueryB2C q) r =
      encodeResult ccfg coleNodeToClientVersion q r
  decodeResult (CodecConfigB2C ccfg) () (QueryB2C (q :: BlockQuery ColeBlock result)) =
      (\(QueryResultSuccess r) -> r) <$>
        decodeResult
          (toBccCodecConfig ccfg)
          bccNodeToClientVersion
          (QueryIfCurrentCole q :: BccQuery
                                      Crypto
                                      (BccQueryResult Crypto result))

instance SerialiseNodeToClientConstraints ColeToBcc

{------------------------------------------------------------------------------
  Cole to Bcc: Arbitrary instances
------------------------------------------------------------------------------}

instance Arbitrary ColeToBcc where
  arbitrary = B2C <$> arbitrary

instance Arbitrary (Header ColeToBcc) where
  arbitrary = HeaderB2C <$> (arbitrary `suchThatMap` isRightVersion)
    where
      isRightVersion ::
           WithVersion ColeNodeToNodeVersion (Header ColeBlock)
        -> Maybe (Header ColeBlock)
      isRightVersion (WithVersion version hdr)
        | version == coleNodeToNodeVersion = Just hdr
        | otherwise                         = Nothing

instance Arbitrary (GenTx ColeToBcc) where
  arbitrary = GenTxB2C <$> arbitrary

instance Arbitrary (GenTxId ColeToBcc) where
  arbitrary = GenTxIdB2C <$> arbitrary

instance Arbitrary (SomeSecond BlockQuery ColeToBcc) where
  arbitrary = (\(SomeSecond q) -> SomeSecond (QueryB2C q)) <$> arbitrary

instance Arbitrary (SomeResult ColeToBcc) where
  arbitrary = (\(SomeResult q r) -> SomeResult (QueryB2C q) r) <$> arbitrary

{------------------------------------------------------------------------------
  Bcc to Cole
------------------------------------------------------------------------------}

-- | Encoded Bcc values can be decoded as Cole values in the following
-- cases:
--
-- * The @HardForkNodeTo(Node|Client)Disabled@ version is used
-- * Blocks and headers stored on disk
--
-- Note that ledger state and all other types stored as part of the ledger
-- snapshot are __not__ forwards compatible.
newtype BccToCole                       = C2B        { unC2B        ::            ColeBlock   } deriving (Eq, Show)
newtype instance Header BccToCole       = HeaderC2B  { unHeaderC2B  :: Header     ColeBlock   } deriving (Eq, Show)
newtype instance GenTx BccToCole        = GenTxC2B   { unGenTxC2B   :: GenTx      ColeBlock   } deriving (Eq, Show)
newtype instance TxId (GenTx BccToCole) = GenTxIdC2B { unGenTxIdC2B :: GenTxId    ColeBlock   } deriving (Eq, Show)
newtype instance BlockQuery BccToCole a = QueryC2B   { unQueryC2B   :: BlockQuery ColeBlock a } deriving (Eq, Show)

newtype instance NestedCtxt_ BccToCole f a where
  NestedCtxt_C2B :: NestedCtxt_ ColeBlock     f a
                 -> NestedCtxt_ BccToCole f a

deriving instance Show (NestedCtxt_ BccToCole Header a)

unNestedCtxt_C2B :: NestedCtxt_ BccToCole f a -> NestedCtxt_ ColeBlock f a
unNestedCtxt_C2B (NestedCtxt_C2B ctxt) = ctxt

type instance HeaderHash BccToCole = HeaderHash ColeBlock
type instance ApplyTxErr BccToCole = ApplyTxErr ColeBlock

instance HasNetworkProtocolVersion BccToCole

instance ConvertRawHash BccToCole where
  toShortRawHash   _ = toShortRawHash   pb
  fromShortRawHash _ = fromShortRawHash pb
  hashSize         _ = hashSize         pb

data instance CodecConfig BccToCole = CodecConfigC2B (CodecConfig ColeBlock)

instance SameDepIndex (NestedCtxt_ BccToCole Header) where
  sameDepIndex (NestedCtxt_C2B ctxt1) (NestedCtxt_C2B ctxt2) =
      sameDepIndex ctxt1 ctxt2

instance HasNestedContent Header BccToCole where
  unnest hdr = case unnest (unHeaderC2B hdr) of
      DepPair ctxt a -> DepPair (mapNestedCtxt NestedCtxt_C2B ctxt) a
  nest (DepPair ctxt a) =
      HeaderC2B $ nest (DepPair (mapNestedCtxt unNestedCtxt_C2B ctxt) a)

instance ShowQuery (BlockQuery BccToCole) where
  showResult (QueryC2B query) = showResult query

instance SameDepIndex (BlockQuery BccToCole) where
  sameDepIndex (QueryC2B q1) (QueryC2B q2) = sameDepIndex q1 q2

{------------------------------------------------------------------------------
  Bcc to Cole: Disk
------------------------------------------------------------------------------}

encodeDiskC2B ::
     forall f bcc c2b.
     ( EncodeDisk (BccBlock Crypto) (f (BccBlock Crypto))
     , Coercible bcc (f (BccBlock Crypto))
     )
  => Proxy f
  -> (c2b -> bcc)
  -> CodecConfig BccToCole
  -> c2b
  -> Encoding
encodeDiskC2B _ toBcc (CodecConfigC2B ccfg) x =
    encodeDisk (toBccCodecConfig ccfg) (toBcc' x)
  where
    toBcc' :: c2b -> f (BccBlock Crypto)
    toBcc' = coerce . toBcc

decodeDiskC2B ::
     forall f cole c2b.
     ( DecodeDisk ColeBlock (f ColeBlock)
     , Coercible cole (f ColeBlock)
     )
  => Proxy f
  -> (cole -> c2b)
  -> CodecConfig BccToCole
  -> forall s. Decoder s c2b
decodeDiskC2B _ fromCole (CodecConfigC2B ccfg) =
    fromCole' <$> decodeDisk ccfg
  where
    fromCole' :: f ColeBlock -> c2b
    fromCole' = fromCole . coerce

instance EncodeDisk BccToCole BccToCole where
  encodeDisk = encodeDiskC2B (Proxy @I) (BlockCole . unC2B)

instance DecodeDisk BccToCole (Lazy.ByteString -> BccToCole) where
  decodeDisk = decodeDiskC2B (Proxy @((->) Lazy.ByteString)) (fmap C2B)

instance EncodeDiskDep (NestedCtxt Header) BccToCole where
  encodeDiskDep (CodecConfigC2B ccfg) =
      encodeDiskDep (toBccCodecConfig ccfg) . mapNestedCtxt (NCZ . unNestedCtxt_C2B)

instance DecodeDiskDep (NestedCtxt Header) BccToCole where
  decodeDiskDep (CodecConfigC2B ccfg) =
      decodeDiskDep ccfg . mapNestedCtxt unNestedCtxt_C2B

{------------------------------------------------------------------------------
  Bcc to Cole: NodeToNode
------------------------------------------------------------------------------}

encodeNodeToNodeC2B ::
     forall f bcc c2b.
     ( SerialiseNodeToNode (BccBlock Crypto) (f (BccBlock Crypto))
     , Coercible bcc (f (BccBlock Crypto))
     )
  => Proxy f
  -> (c2b -> bcc)
  -> CodecConfig BccToCole
  -> BlockNodeToNodeVersion BccToCole
  -> c2b
  -> Encoding
encodeNodeToNodeC2B _ toBcc (CodecConfigC2B ccfg) () x =
    encodeNodeToNode
      (toBccCodecConfig ccfg)
      bccNodeToNodeVersion
      (toBcc' x)
  where
    toBcc' :: c2b -> f (BccBlock Crypto)
    toBcc' = coerce . toBcc

decodeNodeToNodeC2B ::
     forall f cole c2b.
     ( SerialiseNodeToNode ColeBlock (f ColeBlock)
     , Coercible cole (f ColeBlock)
     )
  => Proxy f
  -> (cole -> c2b)
  -> CodecConfig BccToCole
  -> BlockNodeToNodeVersion BccToCole
  -> forall s. Decoder s c2b
decodeNodeToNodeC2B _ fromCole (CodecConfigC2B ccfg) () =
    fromCole' <$> decodeNodeToNode ccfg coleNodeToNodeVersion
  where
    fromCole' :: f ColeBlock -> c2b
    fromCole' = fromCole . coerce

instance SerialiseNodeToNode BccToCole BccToCole where
  encodeNodeToNode = encodeNodeToNodeC2B (Proxy @I) (BlockCole . unC2B)
  decodeNodeToNode = decodeNodeToNodeC2B (Proxy @I) C2B

instance SerialiseNodeToNode BccToCole (Serialised BccToCole) where
  encodeNodeToNode = encodeNodeToNodeC2B (Proxy @Serialised) id
  decodeNodeToNode = decodeNodeToNodeC2B (Proxy @Serialised) id

instance SerialiseNodeToNode BccToCole (SerialisedHeader BccToCole) where
  encodeNodeToNode = encodeNodeToNodeC2B
                       (Proxy @SerialisedHeader)
                       (castSerialisedHeader (\(NestedCtxt_C2B ctxt) -> NCZ ctxt))
  decodeNodeToNode = decodeNodeToNodeC2B
                       (Proxy @SerialisedHeader)
                       (castSerialisedHeader NestedCtxt_C2B)

instance SerialiseNodeToNode BccToCole (Header BccToCole) where
  encodeNodeToNode = encodeNodeToNodeC2B (Proxy @Header) (HeaderCole . unHeaderC2B)
  decodeNodeToNode = decodeNodeToNodeC2B (Proxy @Header) HeaderC2B

instance SerialiseNodeToNode BccToCole (GenTx BccToCole) where
  encodeNodeToNode = encodeNodeToNodeC2B (Proxy @GenTx) (GenTxCole . unGenTxC2B)
  decodeNodeToNode = decodeNodeToNodeC2B (Proxy @GenTx) GenTxC2B

instance SerialiseNodeToNode BccToCole (GenTxId BccToCole) where
  encodeNodeToNode = encodeNodeToNodeC2B (Proxy @WrapGenTxId) (GenTxIdCole . unGenTxIdC2B)
  decodeNodeToNode = decodeNodeToNodeC2B (Proxy @WrapGenTxId) GenTxIdC2B

instance SerialiseNodeToNodeConstraints BccToCole where
  estimateBlockSize = estimateBlockSize . unHeaderC2B

{------------------------------------------------------------------------------
  Bcc to Cole: NodeToClient
------------------------------------------------------------------------------}

encodeNodeToClientC2B ::
     forall f bcc c2b.
     ( SerialiseNodeToClient (BccBlock Crypto) (f (BccBlock Crypto))
     , Coercible bcc (f (BccBlock Crypto))
     )
  => Proxy f
  -> (c2b -> bcc)
  -> CodecConfig BccToCole
  -> BlockNodeToClientVersion BccToCole
  -> c2b
  -> Encoding
encodeNodeToClientC2B _ toBcc (CodecConfigC2B ccfg) () x =
    encodeNodeToClient
      @(BccBlock Crypto)
      (toBccCodecConfig ccfg)
      bccNodeToClientVersion
      (toBcc' x)
  where
    toBcc' :: c2b -> f (BccBlock Crypto)
    toBcc' = coerce . toBcc

decodeNodeToClientC2B ::
     forall f cole c2b.
     ( SerialiseNodeToClient ColeBlock (f ColeBlock)
     , Coercible cole (f ColeBlock)
     )
  => Proxy f
  -> (cole -> c2b)
  -> CodecConfig BccToCole
  -> BlockNodeToClientVersion BccToCole
  -> forall s. Decoder s c2b
decodeNodeToClientC2B _ fromCole (CodecConfigC2B ccfg) () =
    fromCole' <$> decodeNodeToClient @ColeBlock ccfg coleNodeToClientVersion
  where
    fromCole' :: f ColeBlock -> c2b
    fromCole' = fromCole . coerce

instance SerialiseNodeToClient BccToCole BccToCole where
  encodeNodeToClient = encodeNodeToClientC2B (Proxy @I) (BlockCole . unC2B)
  decodeNodeToClient = decodeNodeToClientC2B (Proxy @I) C2B

instance SerialiseNodeToClient BccToCole (Serialised BccToCole) where
  encodeNodeToClient = encodeNodeToClientC2B (Proxy @Serialised) id
  decodeNodeToClient = decodeNodeToClientC2B (Proxy @Serialised) id

instance SerialiseNodeToClient BccToCole (GenTx BccToCole) where
  encodeNodeToClient = encodeNodeToClientC2B (Proxy @GenTx) (GenTxCole . unGenTxC2B)
  decodeNodeToClient = decodeNodeToClientC2B (Proxy @GenTx) GenTxC2B

-- | @'ApplyTxErr' 'BccToCole'@
instance SerialiseNodeToClient BccToCole CC.ApplyMempoolPayloadErr where
  encodeNodeToClient = encodeNodeToClientC2B (Proxy @WrapApplyTxErr) ApplyTxErrCole
  decodeNodeToClient = decodeNodeToClientC2B (Proxy @WrapApplyTxErr) id

instance SerialiseNodeToClient BccToCole (SomeSecond BlockQuery BccToCole) where
  encodeNodeToClient =
      encodeNodeToClientC2B
        (Proxy @(SomeSecond BlockQuery))
        (\(SomeSecond q) -> SomeSecond (QueryIfCurrentCole (unQueryC2B q)))
  decodeNodeToClient =
      decodeNodeToClientC2B
        (Proxy @(SomeSecond BlockQuery))
        (\(SomeSecond q) -> SomeSecond (QueryC2B q))

instance SerialiseResult BccToCole (BlockQuery BccToCole) where
  encodeResult (CodecConfigC2B ccfg) () (QueryC2B q) (r :: result) =
      encodeResult
        (toBccCodecConfig ccfg)
        bccNodeToClientVersion
        (QueryIfCurrentCole q)
        (QueryResultSuccess r :: BccQueryResult Crypto result)
  decodeResult (CodecConfigC2B ccfg) () (QueryC2B q) =
      decodeResult ccfg coleNodeToClientVersion q

instance SerialiseNodeToClientConstraints BccToCole

{------------------------------------------------------------------------------
  Bcc to Cole: Arbitrary instances
------------------------------------------------------------------------------}

instance Arbitrary BccToCole where
  arbitrary = C2B <$> arbitrary

instance Arbitrary (Header BccToCole) where
  arbitrary = HeaderC2B <$> (arbitrary `suchThatMap` isRightVersion)
    where
      isRightVersion ::
           WithVersion ColeNodeToNodeVersion (Header ColeBlock)
        -> Maybe (Header ColeBlock)
      isRightVersion (WithVersion version hdr)
        | version == coleNodeToNodeVersion = Just hdr
        | otherwise                         = Nothing

instance Arbitrary (GenTx BccToCole) where
  arbitrary = GenTxC2B <$> arbitrary

instance Arbitrary (GenTxId BccToCole) where
  arbitrary = GenTxIdC2B <$> arbitrary

instance Arbitrary (SomeSecond BlockQuery BccToCole) where
  arbitrary = (\(SomeSecond q) -> SomeSecond (QueryC2B q)) <$> arbitrary

instance Arbitrary (SomeResult BccToCole) where
  arbitrary = (\(SomeResult q r) -> SomeResult (QueryC2B q) r) <$> arbitrary
