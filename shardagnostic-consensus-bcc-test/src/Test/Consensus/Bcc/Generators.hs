{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | 'Arbitrary' instances intended for serialisation roundtrip tests for
-- 'BccBlock' and its related types.
--
-- Because the generated values are only used in serialisation roundtrip tests,
-- they don't need to be valid blocks, transactions, etc.
--
-- We combine the Cole and Sophie-based instances defined elsewhere into
-- Bcc instances by picking randomly from one of the eras.
module Test.Consensus.Bcc.Generators (module Test.Consensus.Cole.Generators) where

import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Proxy
import           Data.SOP.Strict

import           Test.QuickCheck

import           Test.Bcc.Ledger.Aurum.Serialisation.Generators ()

import           Shardagnostic.Consensus.Block
import qualified Shardagnostic.Consensus.HardFork.History as History
import           Shardagnostic.Consensus.HeaderValidation
import           Shardagnostic.Consensus.Node.NetworkProtocolVersion
import           Shardagnostic.Consensus.Node.Serialisation (Some (..))
import           Shardagnostic.Consensus.TypeFamilyWrappers
import           Shardagnostic.Consensus.Util.Counting (NonEmpty (..),
                     nonEmptyFromList, nonEmptyToList)
import           Shardagnostic.Consensus.Util.SOP (nsFromIndex)

import           Shardagnostic.Consensus.HardFork.Combinator
import           Shardagnostic.Consensus.HardFork.Combinator.Serialisation

import           Shardagnostic.Consensus.Cole.Ledger

import           Shardagnostic.Consensus.Sophie.Ledger

import           Shardagnostic.Consensus.Bcc.Block
import           Shardagnostic.Consensus.Bcc.Node (BccHardForkConstraints)

import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Serialisation.Roundtrip (Coherent (..),
                     WithVersion (..))

import           Test.Consensus.Cole.Generators

import           Test.Consensus.Sophie.Generators
import           Test.Consensus.Sophie.MockCrypto (CanMock)

import           Test.Consensus.Bcc.MockCrypto

{-------------------------------------------------------------------------------
  Disk
-------------------------------------------------------------------------------}

instance Arbitrary (BccBlock MockCryptoCompatCole) where
  arbitrary =
      oneof $ catMaybes $ hcollapse generators
    where
      generators ::
        NP
          (K (Maybe (Gen (BccBlock MockCryptoCompatCole))))
          (BccEras MockCryptoCompatCole)
      generators =
            mk BlockCole
         :* mk BlockSophie
         :* mk BlockEvie
         :* mk BlockJen
         :* mk BlockAurum
         :* Nil

      mk ::
           forall a x. Arbitrary a
        => (a -> BccBlock MockCryptoCompatCole)
        -> K (Maybe (Gen (BccBlock MockCryptoCompatCole))) x
      mk f = K $ Just $ f <$> arbitrary

instance Arbitrary (Coherent (BccBlock MockCryptoCompatCole)) where
  arbitrary =
      fmap Coherent $ oneof $ catMaybes $ hcollapse generators
    where
      generators ::
        NP
          (K (Maybe (Gen (BccBlock MockCryptoCompatCole))))
          (BccEras MockCryptoCompatCole)
      generators =
            mk BlockCole
         :* mk BlockSophie
         :* mk BlockEvie
         :* mk BlockJen
         :* mk BlockAurum
         :* Nil

      mk ::
           forall a x. Arbitrary (Coherent a)
        => (a -> BccBlock MockCryptoCompatCole)
        -> K (Maybe (Gen (BccBlock MockCryptoCompatCole))) x
      mk f = K $ Just $ f . getCoherent <$> arbitrary

instance Arbitrary (BccHeader MockCryptoCompatCole) where
  arbitrary = getHeader <$> arbitrary

instance (CanMock (SophieEra c), BccHardForkConstraints c)
      => Arbitrary (OneEraHash (BccEras c)) where
  arbitrary = inj <$> arbitrary
    where
      inj :: NS WrapHeaderHash (BccEras c) -> OneEraHash (BccEras c)
      inj = hcollapse . hcmap proxySingle aux

      aux ::
           forall blk. SingleEraBlock blk
        => WrapHeaderHash blk -> K (OneEraHash (BccEras c)) blk
      aux = K . OneEraHash . toShortRawHash (Proxy @blk) . unwrapHeaderHash

instance (c ~ MockCryptoCompatCole, SophieBasedEra (SophieEra c))
      => Arbitrary (AnnTip (BccBlock c)) where
  arbitrary = AnnTip
      <$> (SlotNo <$> arbitrary)
      <*> arbitrary
      <*> (OneEraTipInfo <$> arbitrary)

{-------------------------------------------------------------------------------
  NodeToNode
-------------------------------------------------------------------------------}

instance BccHardForkConstraints c
      => Arbitrary (HardForkNodeToNodeVersion (BccEras c)) where
  arbitrary =
    elements $ Map.elems $ supportedNodeToNodeVersions (Proxy @(BccBlock c))

instance Arbitrary (BlockNodeToNodeVersion blk)
     => Arbitrary (EraNodeToNodeVersion blk) where
  arbitrary = frequency
    [ (1, pure EraNodeToNodeDisabled)
    , (9, EraNodeToNodeEnabled <$> arbitrary)
    ]

arbitraryNodeToNode
  :: ( Arbitrary (WithVersion ColeNodeToNodeVersion cole)
     , Arbitrary (WithVersion SophieNodeToNodeVersion sophie)
     , Arbitrary (WithVersion SophieNodeToNodeVersion evie)
     , Arbitrary (WithVersion SophieNodeToNodeVersion jen)
     , Arbitrary (WithVersion SophieNodeToNodeVersion aurum)
     )
  => (cole   -> bcc)
  -> (sophie -> bcc)
  -> (evie -> bcc)
  -> (jen    -> bcc)
  -> (aurum  -> bcc)
  -> Gen (WithVersion (HardForkNodeToNodeVersion (BccEras c)) bcc)
arbitraryNodeToNode injCole injSophie injEvie injJen injAurum = oneof
    -- Cole + HardFork disabled
    [ (\(WithVersion versionCole b) ->
          WithVersion
            (HardForkNodeToNodeDisabled versionCole)
            (injCole b))
        <$> arbitrary
    -- Cole + HardFork enabled.
    -- NOTE: Any value generated by the V1 generator is also fine when using
    -- V2.
    , (\(WithVersion versionCole b) versionSophie versionEvie versionJen versionAurum ->
          WithVersion
            (HardForkNodeToNodeEnabled
              maxBound
              (  EraNodeToNodeEnabled versionCole
              :* EraNodeToNodeEnabled versionSophie
              :* versionEvie
              :* versionJen
              :* versionAurum
              :* Nil
              ))
            (injCole b))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Sophie + HardFork enable
    , (\versionCole (WithVersion versionSophie s) versionEvie versionJen versionAurum ->
          WithVersion
            (HardForkNodeToNodeEnabled
              maxBound
              (  EraNodeToNodeEnabled versionCole
              :* EraNodeToNodeEnabled versionSophie
              :* versionEvie
              :* versionJen
              :* versionAurum
              :* Nil
              ))
            (injSophie s))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Evie + HardFork enabled
    , (\versionCole versionSophie (WithVersion versionEvie a) versionJen versionAurum ->
          WithVersion
            (HardForkNodeToNodeEnabled
              maxBound
              (  EraNodeToNodeEnabled versionCole
              :* EraNodeToNodeEnabled versionSophie
              :* EraNodeToNodeEnabled versionEvie
              :* versionJen
              :* versionAurum
              :* Nil
              ))
            (injEvie a))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Jen + HardFork enabled
    , (\versionCole versionSophie versionEvie (WithVersion versionJen m) versionAurum ->
          WithVersion
            (HardForkNodeToNodeEnabled
              maxBound
              (  EraNodeToNodeEnabled versionCole
              :* EraNodeToNodeEnabled versionSophie
              :* EraNodeToNodeEnabled versionEvie
              :* EraNodeToNodeEnabled versionJen
              :* versionAurum
              :* Nil
              ))
            (injJen m))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Aurum + HardFork enabled
    , (\versionCole versionSophie versionEvie versionJen (WithVersion versionAurum a) ->
          WithVersion
            (HardForkNodeToNodeEnabled
              maxBound
              (  EraNodeToNodeEnabled versionCole
              :* EraNodeToNodeEnabled versionSophie
              :* EraNodeToNodeEnabled versionEvie
              :* EraNodeToNodeEnabled versionJen
              :* EraNodeToNodeEnabled versionAurum
              :* Nil
              ))
            (injAurum a))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    ]

instance c ~ MockCryptoCompatCole
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (BccEras c))
                                (SomeSecond (NestedCtxt Header) (BccBlock c))) where
  arbitrary = arbitraryNodeToNode injCole injSophie injEvie injJen injAurum
    where
      injCole   = mapSomeNestedCtxt NCZ
      injSophie = mapSomeNestedCtxt (NCS . NCZ)
      injEvie = mapSomeNestedCtxt (NCS . NCS . NCZ)
      injJen    = mapSomeNestedCtxt (NCS . NCS . NCS . NCZ)
      injAurum  = mapSomeNestedCtxt (NCS . NCS . NCS . NCS . NCZ)

instance c ~ MockCryptoCompatCole
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (BccEras c))
                                (BccBlock c)) where
  arbitrary = arbitraryNodeToNode BlockCole BlockSophie BlockEvie BlockJen BlockAurum

instance c ~ MockCryptoCompatCole
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (BccEras c))
                                (BccHeader c)) where
  arbitrary = arbitraryNodeToNode HeaderCole HeaderSophie HeaderEvie HeaderJen HeaderAurum

instance c ~ MockCryptoCompatCole
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (BccEras c))
                                (BccGenTx c)) where
  arbitrary = arbitraryNodeToNode GenTxCole GenTxSophie GenTxEvie GenTxJen GenTxAurum

instance c ~ MockCryptoCompatCole
      => Arbitrary (WithVersion (HardForkNodeToNodeVersion (BccEras c))
                                (BccGenTxId c)) where
  arbitrary = arbitraryNodeToNode GenTxIdCole GenTxIdSophie GenTxIdEvie GenTxIdJen GenTxIdAurum

{-------------------------------------------------------------------------------
  NodeToClient
-------------------------------------------------------------------------------}

instance BccHardForkConstraints c
      => Arbitrary (HardForkNodeToClientVersion (BccEras c)) where
  arbitrary =
    elements $ Map.elems $ supportedNodeToClientVersions (Proxy @(BccBlock c))

newtype HardForkEnabledNodeToClientVersion c = HardForkEnabledNodeToClientVersion {
      getHardForkEnabledNodeToClientVersion :: HardForkNodeToClientVersion (BccEras c)
    }

deriving newtype instance BccHardForkConstraints c
                       => Eq (HardForkEnabledNodeToClientVersion c)
deriving newtype instance BccHardForkConstraints c
                       => Show (HardForkEnabledNodeToClientVersion c)

instance BccHardForkConstraints c
      => Arbitrary (HardForkEnabledNodeToClientVersion c) where
  arbitrary =
        elements
      . map HardForkEnabledNodeToClientVersion
      . filter isHardForkNodeToClientEnabled
      . Map.elems
      . supportedNodeToClientVersions
      $ Proxy @(BccBlock c)

-- | Generate a supported 'HardForkNodeToClientVersion' of which the
-- 'HardForkSpecificNodeToClientVersion' satisfies the given predicate.
--
-- PRECONDITION: 'supportedNodeToClientVersions' must include a version that
-- satisfies this condition.
genWithHardForkSpecificNodeToClientVersion ::
     forall c. BccHardForkConstraints c
  => (HardForkSpecificNodeToClientVersion -> Bool)
  -> Gen (HardForkNodeToClientVersion (BccEras c))
genWithHardForkSpecificNodeToClientVersion p =
      elements
    . filter p'
    . Map.elems
    . supportedNodeToClientVersions
    $ Proxy @(BccBlock c)
  where
    p' :: HardForkNodeToClientVersion (BccEras c) -> Bool
    p' (HardForkNodeToClientEnabled v _) = p v
    p' (HardForkNodeToClientDisabled {}) = False

instance Arbitrary (BlockNodeToClientVersion blk)
     => Arbitrary (EraNodeToClientVersion blk) where
  arbitrary = frequency
    [ (1, pure EraNodeToClientDisabled)
    , (9, EraNodeToClientEnabled <$> arbitrary)
    ]

arbitraryNodeToClient
  :: ( Arbitrary (WithVersion ColeNodeToClientVersion   cole)
     , Arbitrary (WithVersion SophieNodeToClientVersion sophie)
     , Arbitrary (WithVersion SophieNodeToClientVersion evie)
     , Arbitrary (WithVersion SophieNodeToClientVersion jen)
     , Arbitrary (WithVersion SophieNodeToClientVersion aurum)
     )
  => (cole   -> bcc)
  -> (sophie -> bcc)
  -> (evie -> bcc)
  -> (jen    -> bcc)
  -> (aurum  -> bcc)
  -> Gen (WithVersion (HardForkNodeToClientVersion (BccEras c)) bcc)
arbitraryNodeToClient injCole injSophie injEvie injJen injAurum = oneof
    -- Cole + HardFork disabled
    [ (\(WithVersion versionCole b) ->
          WithVersion
            (HardForkNodeToClientDisabled versionCole)
            (injCole b))
        <$> arbitrary
    -- Cole + HardFork enabled.
    , (\(WithVersion versionCole b) versionSophie versionEvie versionJen versionAurum ->
          WithVersion
            (HardForkNodeToClientEnabled
              maxBound
              (  EraNodeToClientEnabled versionCole
              :* EraNodeToClientEnabled versionSophie
              :* versionEvie
              :* versionJen
              :* versionAurum
              :* Nil
              ))
            (injCole b))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Sophie + HardFork enabled
    , (\versionCole (WithVersion versionSophie s) versionEvie versionJen versionAurum ->
          WithVersion
            (HardForkNodeToClientEnabled
              maxBound
              (  EraNodeToClientEnabled versionCole
              :* EraNodeToClientEnabled versionSophie
              :* versionEvie
              :* versionJen
              :* versionAurum
              :* Nil
              ))
            (injSophie s))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Evie + HardFork enabled
    , (\versionCole versionSophie (WithVersion versionEvie a) versionJen versionAurum ->
          WithVersion
            (HardForkNodeToClientEnabled
              maxBound
              (  EraNodeToClientEnabled versionCole
              :* EraNodeToClientEnabled versionSophie
              :* EraNodeToClientEnabled versionEvie
              :* versionJen
              :* versionAurum
              :* Nil
              ))
            (injEvie a))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Jen + HardFork enabled
    , (\versionCole versionSophie versionEvie (WithVersion versionJen m) versionAurum ->
          WithVersion
            (HardForkNodeToClientEnabled
              maxBound
              (  EraNodeToClientEnabled versionCole
              :* EraNodeToClientEnabled versionSophie
              :* EraNodeToClientEnabled versionEvie
              :* EraNodeToClientEnabled versionJen
              :* versionAurum
              :* Nil
              ))
            (injJen m))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    -- Aurum + HardFork enabled
    , (\versionCole versionSophie versionEvie versionJen (WithVersion versionAurum a) ->
          WithVersion
            (HardForkNodeToClientEnabled
              maxBound
              (  EraNodeToClientEnabled versionCole
              :* EraNodeToClientEnabled versionSophie
              :* EraNodeToClientEnabled versionEvie
              :* EraNodeToClientEnabled versionJen
              :* EraNodeToClientEnabled versionAurum
              :* Nil
              ))
            (injAurum a))
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    ]

instance c ~ MockCryptoCompatCole
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (BccEras c))
                                (BccBlock c)) where
  arbitrary = arbitraryNodeToClient BlockCole BlockSophie BlockEvie BlockJen BlockAurum

instance c ~ MockCryptoCompatCole
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (BccEras c))
                                (BccGenTx c)) where
  arbitrary = arbitraryNodeToClient GenTxCole GenTxSophie GenTxEvie GenTxJen GenTxAurum

instance c ~ MockCryptoCompatCole
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (BccEras c))
                                (BccApplyTxErr c)) where
  arbitrary = frequency
      [ (8, arbitraryNodeToClient ApplyTxErrCole ApplyTxErrSophie ApplyTxErrEvie ApplyTxErrJen ApplyTxErrAurum)
      , (2, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (HardForkApplyTxErrWrongEra <$> arbitrary))
      ]
  shrink = traverse aux
    where
      aux :: BccApplyTxErr MockCryptoCompatCole
         -> [BccApplyTxErr MockCryptoCompatCole]
      aux (HardForkApplyTxErrFromEra (OneEraApplyTxErr x)) =
          HardForkApplyTxErrFromEra . OneEraApplyTxErr <$> shrink x
      aux (HardForkApplyTxErrWrongEra x) =
          HardForkApplyTxErrWrongEra <$> shrink x

instance Arbitrary (Some QueryAnytime) where
  arbitrary = return $ Some GetEraStart

instance BccHardForkConstraints c
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (BccEras c))
                                (Some (QueryHardFork (BccEras c)))) where
  arbitrary = frequency
      [ (1, do version <- getHardForkEnabledNodeToClientVersion <$> arbitrary
               return $ WithVersion version (Some GetInterpreter))
      , (1, do version <- genWithHardForkSpecificNodeToClientVersion
                            (>= HardForkSpecificNodeToClientVersion2)
               return $ WithVersion version (Some GetCurrentEra))
      ]

instance c ~ MockCryptoCompatCole
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (BccEras c))
                                (SomeSecond BlockQuery (BccBlock c))) where
  arbitrary = frequency
      [ (1, arbitraryNodeToClient injCole injSophie injEvie injJen injAurum)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (injAnytimeCole <$> arbitrary))
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (injAnytimeSophie <$> arbitrary))
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (injAnytimeEvie <$> arbitrary))
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (injAnytimeJen <$> arbitrary))
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> (injAnytimeAurum <$> arbitrary))
      , (1, fmap injHardFork <$> arbitrary)
      ]
    where
      injCole          (SomeSecond query) = SomeSecond (QueryIfCurrentCole   query)
      injSophie        (SomeSecond query) = SomeSecond (QueryIfCurrentSophie query)
      injEvie        (SomeSecond query) = SomeSecond (QueryIfCurrentEvie query)
      injJen           (SomeSecond query) = SomeSecond (QueryIfCurrentJen    query)
      injAurum         (SomeSecond query) = SomeSecond (QueryIfCurrentAurum  query)
      injAnytimeCole   (Some      query)  = SomeSecond (QueryAnytimeCole     query)
      injAnytimeSophie (Some      query)  = SomeSecond (QueryAnytimeSophie   query)
      injAnytimeEvie (Some      query)  = SomeSecond (QueryAnytimeEvie   query)
      injAnytimeJen    (Some      query)  = SomeSecond (QueryAnytimeJen      query)
      injAnytimeAurum  (Some      query)  = SomeSecond (QueryAnytimeAurum    query)
      injHardFork       (Some      query)  = SomeSecond (QueryHardFork         query)

instance Arbitrary History.EraEnd where
  arbitrary = oneof
      [ History.EraEnd <$> arbitrary
      , return History.EraUnbounded
      ]

instance Arbitrary History.SafeZone where
  arbitrary = oneof
      [ History.StandardSafeZone <$> arbitrary
      , return History.UnsafeIndefiniteSafeZone
      ]

instance Arbitrary History.EraParams where
  arbitrary = History.EraParams
      <$> (EpochSize <$> arbitrary)
      <*> arbitrary
      <*> arbitrary

instance Arbitrary History.EraSummary where
  arbitrary = History.EraSummary
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance (Arbitrary a, SListI xs) => Arbitrary (NonEmpty xs a) where
  arbitrary = do
      let nbXs = lengthSList (Proxy @xs)
      len <- choose (1, nbXs)
      xs  <- vectorOf len arbitrary
      return $ fromMaybe (error "nonEmptyFromList failed") $ nonEmptyFromList xs

instance Arbitrary (History.Interpreter (BccEras c)) where
  arbitrary =
      History.mkInterpreter . History.Summary . enforceInvariant <$> arbitrary
    where
      -- Enforce the invariant that when the last era in the summary is the
      -- final era, it is unbounded. The decoder relies on this.
      enforceInvariant xs
        | length (nonEmptyToList xs) == lengthSList (Proxy @(BccEras c))
        = fixEndBound xs
        | otherwise
        = xs

      fixEndBound ::
           NonEmpty xs History.EraSummary
        -> NonEmpty xs History.EraSummary
      fixEndBound (NonEmptyCons e es) = NonEmptyCons e (fixEndBound es)
      fixEndBound (NonEmptyOne  e)    =
          NonEmptyOne  e { History.eraEnd = History.EraUnbounded }

instance Arbitrary (EraIndex (BccEras c)) where
  arbitrary = do
    let nbEras = lengthSList (Proxy @(BccEras c))
    index <- choose (0, fromIntegral nbEras - 1)
    case nsFromIndex index of
      Nothing -> error $ "nsFromIndex failed for " <> show index
      Just ns -> return $ eraIndexFromNS ns

instance c ~ MockCryptoCompatCole
      => Arbitrary (WithVersion (HardForkNodeToClientVersion (BccEras c))
                                (SomeResult (BccBlock c))) where
  arbitrary = frequency
      [ (1, arbitraryNodeToClient injCole injSophie injEvie injJen injAurum)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryIfCurrentResultEraMismatch)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryAnytimeResultCole)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryAnytimeResultSophie)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryAnytimeResultEvie)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryAnytimeResultJen)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryAnytimeResultAurum)
      , (1, WithVersion
              <$> (getHardForkEnabledNodeToClientVersion <$> arbitrary)
              <*> genQueryHardForkResult)
      ]
    where
      injCole   (SomeResult q r) = SomeResult (QueryIfCurrentCole   q) (QueryResultSuccess r)
      injSophie (SomeResult q r) = SomeResult (QueryIfCurrentSophie q) (QueryResultSuccess r)
      injEvie (SomeResult q r) = SomeResult (QueryIfCurrentEvie q) (QueryResultSuccess r)
      injJen    (SomeResult q r) = SomeResult (QueryIfCurrentJen    q) (QueryResultSuccess r)
      injAurum  (SomeResult q r) = SomeResult (QueryIfCurrentAurum  q) (QueryResultSuccess r)

      -- In practice, when sending a Cole query you'll never get a mismatch
      -- saying that your query is from the Sophie era while the ledger is
      -- from Cole. Only the inverse. We ignore that in this generator, as it
      -- doesn't matter for serialisation purposes, we just generate a random
      -- 'MismatchEraInfo'.
      genQueryIfCurrentResultEraMismatch :: Gen (SomeResult (BccBlock c))
      genQueryIfCurrentResultEraMismatch = oneof
          [ (\(SomeResult q (_ :: result)) mismatch ->
                SomeResult (QueryIfCurrentCole q) (Left @_ @result mismatch))
              <$> arbitrary <*> arbitrary
          , (\(SomeResult q (_ :: result)) mismatch ->
                SomeResult (QueryIfCurrentSophie q) (Left @_ @result mismatch))
              <$> arbitrary <*> arbitrary
          , (\(SomeResult q (_ :: result)) mismatch ->
                SomeResult (QueryIfCurrentEvie q) (Left @_ @result mismatch))
              <$> arbitrary <*> arbitrary
          , (\(SomeResult q (_ :: result)) mismatch ->
                SomeResult (QueryIfCurrentJen q) (Left @_ @result mismatch))
              <$> arbitrary <*> arbitrary
          , (\(SomeResult q (_ :: result)) mismatch ->
                SomeResult (QueryIfCurrentAurum q) (Left @_ @result mismatch))
              <$> arbitrary <*> arbitrary
          ]

      genQueryAnytimeResultCole :: Gen (SomeResult (BccBlock c))
      genQueryAnytimeResultCole =
          SomeResult (QueryAnytimeCole GetEraStart) <$> arbitrary

      genQueryAnytimeResultSophie :: Gen (SomeResult (BccBlock c))
      genQueryAnytimeResultSophie =
          SomeResult (QueryAnytimeSophie GetEraStart) <$> arbitrary

      genQueryAnytimeResultEvie :: Gen (SomeResult (BccBlock c))
      genQueryAnytimeResultEvie =
          SomeResult (QueryAnytimeEvie GetEraStart) <$> arbitrary

      genQueryAnytimeResultJen :: Gen (SomeResult (BccBlock c))
      genQueryAnytimeResultJen =
          SomeResult (QueryAnytimeJen GetEraStart) <$> arbitrary

      genQueryAnytimeResultAurum :: Gen (SomeResult (BccBlock c))
      genQueryAnytimeResultAurum =
          SomeResult (QueryAnytimeAurum GetEraStart) <$> arbitrary

      genQueryHardForkResult :: Gen (SomeResult (BccBlock c))
      genQueryHardForkResult = oneof
          [ SomeResult (QueryHardFork GetInterpreter) <$> arbitrary
          , SomeResult (QueryHardFork GetCurrentEra)  <$> arbitrary
          ]
