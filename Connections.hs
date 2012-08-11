{-# LANGUAGE GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
-- File: Connections.hs
-- Creation Date: Aug 06 2012 [01:08:29]
-- Last Modified: Aug 09 2012 [16:44:57]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
-- | A connection has a handle and metadata.
module Connections
  ( Con
  , runCon
  , ConData(..)
  , write
  , writeRaw
  , writeText
  , readRaw
  ) where

import Control.Monad.Reader
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as B
import System.IO (Handle)
import Network.SimpleIRC

-- | A single connection to a IRC network
data ConData = ConData { conSocket      :: Handle
                       , conNetworkName :: T.Text
                       , conNick        :: T.Text
                       }

newtype Con a = Con { runC :: ReaderT ConData IO a }
  deriving (Monad, MonadIO, MonadReader ConData)

runCon :: ConData -> Con a -> IO a
runCon d f = runReaderT (runC f) d

writeRaw :: B.ByteString -> Con ()
writeRaw bs = asks conSocket >>= liftIO . flip B.hPutStrLn bs

writeText :: T.Text -> Con ()
writeText text = asks conSocket >>= liftIO . flip B.hPutStrLn (encodeUtf8 text)

write :: Command -> Con ()
write = writeRaw . showCommand

readRaw :: Con B.ByteString
readRaw = asks conSocket >>= liftIO . B.hGetLine >>= \line -> liftIO (B.putStrLn line) >> return line
