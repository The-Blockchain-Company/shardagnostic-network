{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- TODO where to put this?
module Shardagnostic.Consensus.Sophie.Ledger.TOptimum () where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Bcc.Crypto.VRF (certifiedOutput)

import           Shardagnostic.Consensus.Block
import           Shardagnostic.Consensus.Protocol.Signed

import qualified Sophie.Spec.Ledger.API as SL

import           Shardagnostic.Consensus.Sophie.Eras (EraCrypto)
import           Shardagnostic.Consensus.Sophie.Ledger.Block
import           Shardagnostic.Consensus.Sophie.Ledger.Config
import           Shardagnostic.Consensus.Sophie.Protocol

{-------------------------------------------------------------------------------
  Support for Transitional Optimum consensus algorithm
-------------------------------------------------------------------------------}

type instance BlockProtocol (SophieBlock era) = TOptimum (EraCrypto era)

instance SophieBasedEra era => BlockSupportsProtocol (SophieBlock era) where
  validateView _cfg (SophieHeader hdr _) = hdr

  selectView cfg hdr@(SophieHeader shdr _) = TOptimumChainSelectView {
        csvChainLength = blockNo hdr
      , csvSlotNo      = blockSlot hdr
      , csvSelfIssued  = selfIssued
      , csvIssuer      = SL.bheaderVk hdrBody
      , csvIssueNo     = SL.ocertN . SL.bheaderOCert $ hdrBody
      , csvLeaderVRF   = certifiedOutput . SL.bheaderL $ hdrBody
      }
    where
      hdrBody :: SL.BHBody (EraCrypto era)
      hdrBody = SL.bhbody shdr

      issuerVKeys :: Map (SL.KeyHash 'SL.BlockIssuer (EraCrypto era))
                         (SL.VKey 'SL.BlockIssuer (EraCrypto era))
      issuerVKeys = sophieBlockIssuerVKeys cfg

      -- | Premature optimisation: we assume everywhere that 'selectView' is
      -- cheap, so micro-optimise checking whether the issuer vkey is one of our
      -- own vkeys.
      --
      -- * Equality of vkeys takes roughly 40ns
      -- * Hashing a vkey takes roughly 850ns
      -- * Equality of hashes takes roughly 10ns
      --
      -- We want to avoid the hashing of a vkey as it is more expensive than
      -- simply doing a linear search, comparing vkeys for equality. Only when
      -- we have to do a linear search across a large number of vkeys does it
      -- become more efficient to first hash the vkey and look up its hash in
      -- the map.
      --
      -- We could try to be clever and estimate the number of keys after which
      -- we switch from a linear search to hashing + a O(log n) map lookup, but
      -- we keep it (relatively) simple and optimise for the common case: 0 or 1
      -- key.
      selfIssued :: SelfIssued
      selfIssued = case Map.size issuerVKeys of
          -- The most common case: a non-block producing node
          0 -> NotSelfIssued
          -- A block producing node with a single set of credentials: just do an
          -- equality check of the single VKey, skipping the more expensive
          -- computation of the hash.
          1 | SL.bheaderVk hdrBody `elem` issuerVKeys
            -> SelfIssued
            | otherwise
            -> NotSelfIssued
          -- When we are running with multiple sets of credentials, which should
          -- only happen when benchmarking, do a hash lookup, as the number of
          -- keys can grow to 100-250.
          _ | SL.hashKey (SL.bheaderVk hdrBody) `Map.member` issuerVKeys
            -> SelfIssued
            | otherwise
            -> NotSelfIssued

-- TODO correct place for these two?
type instance Signed (Header (SophieBlock era)) = SL.BHBody (EraCrypto era)

instance SophieBasedEra era => SignedHeader (Header (SophieBlock era)) where
  headerSigned = SL.bhbody . sophieHeaderRaw
