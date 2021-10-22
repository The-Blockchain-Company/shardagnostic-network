module Shardagnostic.Consensus.BlockchainTime.WallClock.Default (defaultSystemTime) where

import           Control.Monad
import           Control.Tracer
import           Data.Time (UTCTime, diffUTCTime)

import           Control.Monad.Class.MonadTime (MonadTime (..))

import           Shardagnostic.Consensus.BlockchainTime.WallClock.Types
import           Shardagnostic.Consensus.BlockchainTime.WallClock.Util
import           Shardagnostic.Consensus.Util.IOLike
import           Shardagnostic.Consensus.Util.Time

defaultSystemTime :: (MonadTime m, MonadDelay m)
                  => SystemStart
                  -> Tracer m (TraceBlockchainTimeEvent UTCTime)
                  -> SystemTime m
defaultSystemTime start tracer = SystemTime {
      systemTimeCurrent = toRelativeTime start <$> getCurrentTime
    , systemTimeWait    = waitForSystemStart start tracer
    }

-- | Wait until system start if necessary
waitForSystemStart :: (MonadTime m, MonadDelay m)
                   => SystemStart
                   -> Tracer m (TraceBlockchainTimeEvent UTCTime)
                   -> m ()
waitForSystemStart start tracer = do
    now <- getCurrentTime
    when (getSystemStart start > now) $ do
      let delay = getSystemStart start `diffUTCTime` now
      traceWith tracer $ TraceStartTimeInTheFuture start delay
      threadDelay (nominalDelay delay)
