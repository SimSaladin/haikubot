{-# LANGUAGE GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
-- File: Haiku.hs
-- Creation Date: Jul 23 2012 [11:10:43]
-- Last Modified: Aug 06 2012 [07:15:20]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
-- |  XXX: support daemon-mode?
module Haiku
  ( mainWithCLI, quit
  , spawn, connect, disconnect, listen
  , log, logRes, raise
  ) where

import           Prelude hiding (catch, getLine, putStr, putStrLn, words, log)
import           Data.ByteString (ByteString) -- so we hide the IsString instance
import qualified Data.ByteString.Char8 as B
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import           Network
import qualified Network.SimpleIRC as IRC
import           Control.Exception hiding (Handler)
import           Control.Monad.Reader
import           Control.Concurrent
import           Text.Printf
import           System.IO

import Utils
import Handler
import Connections

-- | Create and execute a new handler using the supplied configuration, and
--   start the command-line interface.
mainWithCLI :: Config -> IO ()
mainWithCLI config = do
    plugins <- sequence $ cPlugins config
    runHandler runCLI $ Persist plugins

runCLI :: Handler ()
runCLI = forever $
    liftIO (TIO.putStr "haikubot> " >> hFlush stdout >> TIO.getLine)
    >>= rootCommand >>= logRes

rootCommand :: Text -> Handler Result
rootCommand input = let (cmd, args) = cmdSplit input
                        in getPlugins >>= execRoot cmd args

-- | connect to server, fork, listen.
spawn :: Persist
      -> String
      -> Integer
      -> (Handle -> Handler a) -- ^ callback "when connected"
      -> (a -> Handler ())     -- ^ callback "disconnected"
      -> (a -> Handler ())      -- ^ callback "listening"
      -> IO ThreadId
spawn config server port connected listener disconnected = do
    h <- notify $ connectTo server (PortNumber $ fromIntegral port)
    hSetBuffering h NoBuffering
    forkIO $ bracket (      runHandler (connected h)    config)
                     (\x -> runHandler (disconnected x) config)
                     (\x -> runHandler (listener x)     config)
  where
    notify = bracket_ (printf "Connecting to %s... " server >> hFlush stdout)
                      (TIO.putStrLn "Connected")

-- | Create a new connection, greet the server and and store the connection for
-- later access.
connect :: ByteString -- ^ nick
        -> ByteString -- ^ user
        -> ByteString -- ^ address
        -> ByteString -- ^ real name
        -> Handle
        -> Handler ConData
connect nick user addr real h = do
    insert c
    io $ runCon c greet
    return c
  where
    c = ConData h "" ""
    greet = do
      writeRaw $ "NICK " `B.append` nick
      writeRaw $ B.concat ["USER ",user," ",user," ",addr," :",real]

-- | TODO: use an id!
disconnect :: ConData -> Handler ()
disconnect _ = raise "CRITICAL: disconnect is not implemented"

-- | Begin connection listener in a new thread.
listen :: ConData -> Handler ()
listen c = getPlugins >>= \plugins -> forever $ do
  with c (readRaw >>= flip execUni plugins . IRC.parse)

-- | Quits haikubot.
quit :: Handler ()
quit = raise "Quit not implemented"


-- * Logging / Maintainance

logRes :: Result -> Handler ()
logRes (ResFailure msg) = raise $ encodeUtf8 msg
logRes (ResSuccess msg) = log $ encodeUtf8 msg
logRes _                = return ()

log :: B.ByteString -> Handler ()
log = io . B.putStrLn . ("[log] " `B.append`)

raise :: B.ByteString -> Handler ()
raise = io . B.putStrLn . ("[warn] " `B.append`)
