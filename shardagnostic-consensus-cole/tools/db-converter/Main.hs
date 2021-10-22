{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

-- | Database conversion tool.
module Main (main) where

import           Control.Monad.Except (liftIO, runExceptT)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString as BS
import           Data.Foldable (for_)
import           Data.List (sort)
import qualified Data.Text as T
import           Data.Word (Word32, Word64)
import           Options.Generic
import qualified Streaming.Prelude as S
import           System.Directory (canonicalizePath, createDirectoryIfMissing,
                     listDirectory)
import           System.FilePath (takeExtension, takeFileName, (-<.>), (</>))
import qualified System.IO as IO

import qualified Bcc.Binary as CB
import qualified Bcc.Chain.Epoch.File as CC
import           Bcc.Chain.Slotting (EpochSlots (..))

import           Shardagnostic.Network.Magic

import           Shardagnostic.Consensus.Node.DbMarker (dbMarkerContents,
                     dbMarkerFile)
import           Shardagnostic.Consensus.Util.Orphans ()

import qualified Shardagnostic.Consensus.Cole.Ledger as Cole

data Args w = Args
    { epochDir        :: w ::: FilePath     <?> "Path to the directory containing old epoch files"
    , dbDir           :: w ::: FilePath     <?> "Path to the new database directory"
    , epochSlots      :: w ::: Word64       <?> "Slots per epoch"
    , networkMagic    :: w ::: Maybe Word32 <?> "Magic Id of the network"
    }
  deriving (Generic)

instance ParseRecord (Args Wrapped)

deriving instance Show (Args Unwrapped)

main :: IO ()
main = do
    Args {epochDir, dbDir, epochSlots, networkMagic} <- unwrapRecord "Cole DB converter"
    dbDir'    <- canonicalizePath dbDir
    epochDir' <- canonicalizePath epochDir
    files     <- listDirectory epochDir'
    let magic = mkNetworkMagic networkMagic
        epochFiles = sort $ filter (\f -> takeExtension f == ".epoch") files
    putStrLn $ "Writing to " <> show dbDir
    for_ epochFiles $ \f -> do
      putStrLn $ "Converting file " <> show f
      convertChunkFile (EpochSlots epochSlots) (epochDir' </> f) dbDir' magic

-- | If 'NetworkMagic' is not specified, we assume by default it's mainnet.
mkNetworkMagic :: Maybe Word32 -> NetworkMagic
mkNetworkMagic Nothing  = NetworkMagic 764824073
mkNetworkMagic (Just w) = NetworkMagic w

convertChunkFile
  :: EpochSlots
  -> FilePath  -- ^ Absolute input file
  -> FilePath  -- ^ Absolute ouput directory
  -> NetworkMagic
  -> IO (Either CC.ParseError ())
convertChunkFile es inFile outDir magicId = do
    createDirectoryIfMissing True dbDir

    BS.writeFile (outDir </> T.unpack dbMarkerFile) $
      dbMarkerContents magicId

    IO.withFile outFile IO.WriteMode $ \h ->
      runResourceT $ runExceptT $
        S.mapM_ (liftIO . BS.hPut h) . S.map encode $ inStream
  where
    inStream = CC.parseEpochFileWithBoundary es inFile

    dbDir = outDir </> "immutable"

    -- Old filename format is XXXXX.epoch, new is XXXXX.chunk
    outFile = dbDir </> takeFileName inFile -<.> "chunk"

    encode =
        CB.serializeEncoding'
      . Cole.encodeColeBlock
      . Cole.mkColeBlock es
