{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, OverloadedStrings #-}
------------------------------------------------------------------------------
-- File:          Internal.hs
-- Creation Date: Dec 29 2012 [20:19:14]
-- Last Modified: Dec 31 2012 [07:21:37]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Internal.Connections
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
import qualified Data.Text.IO           as T
import qualified Data.Map               as Map
import           Control.Concurrent     (forkIO, ThreadId)
import           Control.Concurrent.STM
import           Control.Exception      (bracket_)
import           Control.Monad
import           Control.Monad.Reader
import           Text.Printf            (printf)
import           Network                ( PortID, connectTo )
import           System.IO              ( hSetBuffering, hFlush
                                        , BufferMode(NoBuffering), stdout
                                        )
import           Internal.Types
import           Internal.Messages


makeConnection :: ConId   -- ^ Identifier to use
               -> String  -- ^ Server
               -> PortID  -- ^ Port
               -> (Con -> Handler ()) -- ^ Listener
               -> Handler (Either Text ThreadId) -- ^ ConId of the newly created connection.
makeConnection conId server port listen = do
    -- check if we already have a connection with a name
    exists <- liftM (Map.member conId) getConnections
    f <- liftM (flip runHandler') getBotData
    if exists
      then return $ Left "Connection with same identifier exists!"
      else liftM Right $ liftIO $ do
          h <- notified $ connectTo server port
          hSetBuffering h NoBuffering
          let con = Con h server port ""
          forkIO $ bracket_
              (f $ insertCon conId con)
              (f $ deleteCon conId con)
              (f $ listen con)
  where
    notified = bracket_
      (printf "Connecting to %s..." server >> hFlush stdout)
      (T.putStrLn "Connected")

getCon :: ConId -> Handler (Maybe Con)
getCon conId = liftM (Map.lookup conId) getConnections 

readLine :: ConId -> Handler (Maybe Text)
readLine conId = getCon conId >>= \x -> case x of
    Nothing  -> return Nothing
    Just con -> fmap Just $ readLine' con

readLine' :: Con -> Handler Text
readLine' = liftIO . T.hGetLine . conSocket

-- | Write a raw text to a Con.
writeLine :: Text -> ConId -> Handler ()
writeLine t conId = getCon conId >>= \x -> case x of
    Nothing  -> return ()
    Just con -> writeLine' t con

writeCmd :: Command -> ConId -> Handler ()
writeCmd = writeLine . showCommand

-- | Write a raw text to a Con.
writeLine' :: Text -> Con -> Handler ()
writeLine' t con = liftIO $ T.hPutStrLn (conSocket con) t

writeCmd' :: Command -> Con -> Handler ()
writeCmd' = writeLine' . showCommand

insertCon :: ConId -> Con -> Handler ()
insertCon conId con = onConnections (\cs -> (Map.insert conId con cs, ()))

deleteCon :: ConId -> Con -> Handler ()
deleteCon conId _ = do
  -- XXX: actions with con?
  onConnections (\cs -> (Map.delete conId cs, ()))

onConnections :: (Connections -> (Connections, a)) -> Handler a
onConnections f = liftIO =<< asks (atomically . action . botConnections) where
  action v = do (v', r) <- liftM f (readTVar v)
                writeTVar v v'
                return r

-- | Efficiently get connections.
getConnections :: Handler Connections
getConnections = liftIO =<< asks (readTVarIO . botConnections)
