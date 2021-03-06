{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Shardagnostic.Consensus.Sophie.Ledger.PeerSelection () where

import           Data.Bifunctor (second)
import           Data.Foldable (toList)
import           Data.List (sortOn)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, mapMaybe)
import           Data.Ord (Down (..))
import           Data.Text.Encoding (encodeUtf8)

import           Shardagnostic.Consensus.Ledger.SupportsPeerSelection

import           Bcc.Ledger.BaseTypes
import qualified Bcc.Ledger.Keys as SL
import qualified Bcc.Protocol.TOptimum as SL
import qualified Sophie.Spec.Ledger.LedgerState as SL
import qualified Sophie.Spec.Ledger.TxBody as SL

import           Shardagnostic.Consensus.Sophie.Eras (EraCrypto)
import           Shardagnostic.Consensus.Sophie.Ledger.Block
import           Shardagnostic.Consensus.Sophie.Ledger.Ledger

instance c ~ EraCrypto era
      => LedgerSupportsPeerSelection (SophieBlock era) where
  getPeers SophieLedgerState { sophieLedgerState } = catMaybes
      [ (poolStake,) <$> Map.lookup stakePool poolRelayAddresses
      | (stakePool, poolStake) <- orderByStake poolDistr
      ]
    where
      poolDistr :: SL.PoolDistr c
      poolDistr = SL.nesPd sophieLedgerState

      -- | Sort stake pools by descending stake
      orderByStake ::
           SL.PoolDistr c
        -> [(SL.KeyHash 'SL.StakePool c, PoolStake)]
      orderByStake =
            sortOn (Down . snd)
          . map (second (PoolStake . SL.individualPoolStake))
          . Map.toList
          . SL.unPoolDistr

      futurePoolParams, poolParams ::
           Map (SL.KeyHash 'SL.StakePool c) (SL.PoolParams c)
      (futurePoolParams, poolParams) =
          (SL._fPParams pstate, SL._pParams pstate)
        where
          pstate :: SL.PState c
          pstate =
                SL._pstate
              . SL._delegationState
              . SL.esLState
              . SL.nesEs
              $ sophieLedgerState

      relayToRelayAddress :: SL.StakePoolRelay -> Maybe RelayAddress
      relayToRelayAddress (SL.SingleHostAddr (SJust (Port port)) (SJust ipv4) _) =
          Just $ RelayAddressAddr (IPv4 ipv4) (fromIntegral port)
      relayToRelayAddress (SL.SingleHostAddr (SJust (Port port)) SNothing (SJust ipv6)) =
          Just $ RelayAddressAddr (IPv6 ipv6) (fromIntegral port)
      relayToRelayAddress (SL.SingleHostName (SJust (Port port)) dnsName) =
          Just $ RelayAddressDomain $ DomainAddress (encodeUtf8 $ dnsToText dnsName) (fromIntegral port)
      relayToRelayAddress _ =
          -- This could be an unsupported relay (SRV records) or an unusable
          -- relay such as a relay with an IP address but without a port number.
          Nothing

      -- | Note that a stake pool can have multiple registered relays
      pparamsRelayAddresses ::
           (RelayAddress -> StakePoolRelay)
        -> SL.PoolParams c
        -> Maybe (NonEmpty StakePoolRelay)
      pparamsRelayAddresses injStakePoolRelay =
            NE.nonEmpty
          . mapMaybe (fmap injStakePoolRelay . relayToRelayAddress)
          . toList
          . SL._poolRelays

      -- | Combine the stake pools registered in the future and the current pool
      -- parameters, and remove duplicates.
      poolRelayAddresses ::
           Map (SL.KeyHash 'SL.StakePool c) (NonEmpty StakePoolRelay)
      poolRelayAddresses =
          Map.unionWith
            (\futureRelays currentRelays -> NE.nub (futureRelays <> currentRelays))
            (Map.mapMaybe (pparamsRelayAddresses FutureRelay)  futurePoolParams)
            (Map.mapMaybe (pparamsRelayAddresses CurrentRelay) poolParams)
