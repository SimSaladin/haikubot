{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, OverloadedStrings #-}
------------------------------------------------------------------------------
-- File:          Haikubot.hs
-- Creation Date: Dec 29 2012 [20:19:14]
-- Last Modified: Oct 09 2013 [00:36:36]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Haikubot.Connections
  ( makeConnection
  , getCon

  , readLine
  , readLine'
  , writeLine
  , writeCmd
  , writeLine'
  , writeCmd'
  ) where

import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Map               as Map
import           Control.Applicative
import           Data.Foldable as Foldable
import           Control.Concurrent     (forkIO, ThreadId)
import           Control.Concurrent.STM
import           Control.Exception      (bracket_, handle, SomeException)
import           Control.Monad
import           Text.Printf            (printf)
import           Network                ( PortID, connectTo )
import           System.IO
import           Haikubot.Core
import           Haikubot.Messages
import           Haikubot.Logging

-- * Handle connections

insertCon :: ConId -> Con -> Handler ()
insertCon conId con = onConnections (\cs -> (Map.insert conId con cs, ()))

deleteCon :: ConId -> Con -> Handler ()
deleteCon conId _ =
  -- XXX: actions with con?
  onConnections (\cs -> (Map.delete conId cs, ()))

onConnections :: (Connections -> (Connections, a)) -> Handler a
onConnections f = liftIO . atomically . action . botConnections =<< getBotData
    where
  action v = do (v', r) <- liftM f (readTVar v)
                writeTVar v v'
                return r

-- | Get all connections.
getConnections :: Handler Connections
getConnections = liftIO . readTVarIO . botConnections =<< getBotData

makeConnection :: ConId   -- ^ Identifier to use
               -> String  -- ^ Server
               -> PortID  -- ^ Port
               -> (Con -> Handler ()) -- ^ Listener
               -> Handler (Either Text ThreadId)
makeConnection conId server port listen = do
    connections <- getConnections
    botData <- getBotData
    if Map.member conId connections
      then return $ Left "Connection with same identifier exists!"
      else liftM Right . liftIO . forkIO . forever . handle onError $ do

              h <- notified $ connectTo server port
              hSetBuffering h NoBuffering
              hSetEncoding h utf8

              let runner = flip runHandler' botData
                  go     = bracket_ <$> runner . insertCon conId
                                    <*> runner . deleteCon conId
                                    <*> runner . listen

              go $ Con h server port ""
  where
    notified = bracket_ (printf "Connecting to %s..." server >> hFlush stdout)
                        (T.putStrLn "Connected")

    onError :: SomeException -> IO ()
    onError err = printf "Server %s connection closed: %s " server (show err) -- TODO: info lvl log msg

getCon :: ConId -> Handler (Maybe Con)
getCon conId = liftM (Map.lookup conId) getConnections 

-- * Read

readLine :: ConId -> Handler (Maybe Text)
readLine conId = getCon conId >>= maybe (return Nothing)
                                        (liftM Just . readLine')

readLine' :: Con -> Handler Text
readLine' = liftIO . T.hGetLine . conSocket

-- * Write

writeCmd :: Command -> ConId -> Handler ()
writeCmd = writeLine . showCommand

writeLine :: Text -> ConId -> Handler ()
writeLine t conId = getCon conId >>= (`Foldable.forM_` writeLine' t)

writeCmd' :: Command -> Con -> Handler ()
writeCmd' = writeLine' . showCommand

-- | Write a raw line of text to a Con.
writeLine' :: Text -> Con -> Handler ()
writeLine' msg con = let (msg', remainder) = T.splitAt 510 msg
    in do unless (T.null remainder) (logErr "note: truncated message larger than 510 characters")
          liftIO $ T.hPutStrLn (conSocket con) msg'
