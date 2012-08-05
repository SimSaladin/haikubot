{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
------------------------------------------------------------------------------
-- File: Haiku.hs
-- Creation Date: Jul 23 2012 [11:10:43]
-- Last Modified: Aug 05 2012 [22:20:27]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

module Haiku
  ( mainWithCLI
  ) where

import           Prelude hiding (catch, getLine, putStr, putStrLn, words, log)
import qualified Data.ByteString as B
import           Data.List (delete)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Network
import qualified Network.SimpleIRC as IRC
import           System.Exit (exitWith, ExitCode(..))
import           System.IO
import           Control.Exception hiding (Handler)
import           Control.Monad.Reader
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Arrow ((***))
import           Text.Printf

import Handler
import Config

-- | Create and execute a new handler using the supplied configuration, and
--   start the command-line interface. XXX: support daemon-mode?
mainWithCLI :: Config -> IO ()
mainWithCLI config = do
    plugins <- sequence $ configPlugins config
    runHandler runCLI $ Persist plugins

-- * Network operations

-- | connect to server, fork, listen.
spawn :: Persist
      -> String
      -> Integer
      -> (Handle -> Handler a) -- ^ callback "when connected"
      -> (a -> Handler b)      -- ^ callback "listening"
      -> (a -> Handler ())     -- ^ callback "disconnected"
      -> IO ThreadId
spawn config server port connected listen disconnected = do
    h <- notify $ connectTo server (PortNumber $ fromIntegral port)
    hSetBuffering h NoBuffering
    forkIO $ bracket (runHandler (connected h)  config)
                     (\x -> runHandler (listen x)       config)
                     (\x -> runHandler (disconnected x) config)
  where
    notify = bracket_ (printf "Connecting to %s... " server >> hFlush stdout)
                      (TIO.putStrLn "Connected")

write :: Connection -> IRC.Command -> IO ()
write con = B.hPutStrLn (conSocket con) . IRC.showCommand 

writeRaw :: Connection -> B.ByteString -> IO ()
writeRaw con bs = B.putStrLn bs >> B.hPutStrLn (conSocket con) bs


-- * Fundamentals

cmdConnect :: Text -> Handler Result
cmdConnect args 
  | length params >= 6 = do
    config <- getPersist
    tid <- liftIO $ spawn config server port init listen dropBot
    return $ ResSuccess $ T.pack $ show tid
  | otherwise          = return $ ResSuccess $ getUsage "connect"
  where
    params = T.words args
    server = T.unpack (params !! 0)
    port   = read $ T.unpack (params !! 1)
    init   = initCon (encodeUtf8 $ params !! 2) (encodeUtf8 $ params !! 3) (encodeUtf8 $ params !! 4) (encodeUtf8 $ T.concat $ drop 5 params)

runCLI :: Handler ()
runCLI = forever $
    liftIO (TIO.putStr "haikubot> " >> hFlush stdout >> TIO.getLine)
    >>= execCommand >>= log

log :: Result -> Handler ()
log r = liftIO $ case r of ResNone        -> return ()
                           ResSuccess msg -> TIO.putStrLn msg
                           ResFailure msg -> TIO.putStrLn msg

-- | Executes any command.
execCommand :: Text -> Handler Result
execCommand input
    | T.null input' = return ResNone
    | prefix == '@' = case cmd of
        "quit"    -> disconnectAndQuit >> return ResNone
        "connect" -> cmdConnect args
        _         -> failed
    | otherwise = return ResNone
  where
    input' = T.strip input
    prefix = T.head input'
    cmd    = T.takeWhile (/= ' ') $ T.tail input'
    args   = T.stripStart $ T.dropWhile (/= ' ') input'

    failed = return $ ResFailure ("Command not found: " `T.append` cmd)


-- * Connection handling

-- | Create a new Connection with supplied handle and put it in the TMVar.
initCon :: B.ByteString -- ^ nick
        -> B.ByteString -- ^ user
        -> B.ByteString -- ^ address
        -> B.ByteString -- ^ real name
        -> Handle
        -> Handler Connection
initCon nick user addr real h = do
    withConnections (\v -> readTVar v >>= writeTVar v . (c :)) >> return c
    io $ B.hPutStrLn h $ "NICK " `B.append` nick
    io $ B.hPutStrLn h $ B.concat ["USER ",user," ",user," ",addr," :",real]
    return c
  where
    c = Connection h "" ""

dropBot :: Connection -> Handler ()
dropBot bot = undefined

listen :: Connection -> Handler ()
listen c = forever $ do
    bs <- io $ B.hGetLine h
    io (putStrLn (show (IRC.parse bs)))
    handleMsg c $ IRC.parse bs
  where
    h = conSocket c

-- | XXX: Should take the connectionId instead
handleMsg :: Connection -> IRC.IrcMessage -> Handler ()
handleMsg con msg = case IRC.mCode msg of
  "PING"    -> io $ writeRaw con $ "PONG :" `B.append` (IRC.mMsg msg)
  "PRIVMSG" -> return ()
  _         -> return ()

getUsage :: Text -> Text
getUsage "connect" = "connect <server> <port> <nick> <user> <addr> <{realname}>"
getUsage t         = t `T.append` ": no help available"

-- | run disconnect hooks on every plugin, close handles (which kills the
-- threads).
disconnectAndQuit :: Handler ()
disconnectAndQuit = -- TODO
    return ()


-- * Helpers

if' :: Bool -> a -> a -> a
if' test t f = if test then t else f

