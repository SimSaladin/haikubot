{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, OverloadedStrings #-}
------------------------------------------------------------------------------
-- File:          Internal.hs
-- Creation Date: Dec 29 2012 [20:19:14]
-- Last Modified: Dec 30 2012 [03:56:26]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Internal.Connections
  ( makeConnection
  , getCon
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
import Internal.Types

-- * Handler functions

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


-- * Helpers

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
