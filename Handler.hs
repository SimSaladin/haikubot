{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification, OverloadedStrings #-}
------------------------------------------------------------------------------
-- File: Handler.hs
-- Creation Date: Aug 05 2012 [05:15:24]
-- Last Modified: Aug 19 2012 [11:56:51]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Handler
  -- * Control
  ( Handler
  , runHandler

  -- * Configuration
  , Config(..)
  , Persist(..)
  , Result(..)

  -- * Plugin stuff
  , Plugin(..)
  , Shared
  , share
  , getShare
  , setShare
  , pluginUni_
  , pluginRoot_

  -- * Interaction
  , execPluginUni
  , execPluginRoot
  , execPluginRoot'
  , execPluginQuit
  , execRoot
  , execUni

  -- * Handler functions
  , with
  , insert
  , withConnections
  , consMap
  , getPersist
  , asksPersist
  , getPlugins
  ) where

import Prelude hiding (log)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.SimpleIRC
import Control.Monad.Reader
import Control.Concurrent.STM

import Utils
import Connections

-- | Network connections are carried in the Handler monad.
-- XXX: should be an a id-value pair?
type Connections = TVar [ConData]

-- | Handler provides access to all connections, configuration. The generic
--   monad for plugins.
newtype Handler a = Handler {
    runH :: ReaderT (Maybe ConData, Connections) (ReaderT Persist IO) a
  } deriving (Monad, MonadIO, MonadReader (Maybe ConData, Connections))

-- |
runHandler :: Handler a -> Persist -> IO a
runHandler h config = do
    cons <- atomically $ newTVar []
    runReaderT (runReaderT (runH h) (Nothing, cons)) config

data Config = Config
  { cRootPrefix :: ByteString
  , cPlugins    :: [IO (Text, Plugin)]
  }

-- | 
data Persist = Persist { persistPlugins    :: Map Text Plugin
                       , persistRootPrefix :: ByteString
                       }

-- |
data Result = ResNone | ResSuccess Text | ResFailure Text


-- * Plugin stuff

-- | XXX: rewrite with typeclasses?
data Plugin = forall env. Plugin
  { pluginPersist :: Shared env
  , pluginUni     :: Shared env -> IrcMessage -> Handler Result
  , pluginRoot    :: Shared env -> Text -> Text -> Maybe IrcMessage -> Handler Result
--, pluginAny     :: Shared env -> IrcMessage -> Handler Result
  }

pluginUni_ :: a -> b -> Handler Result
pluginUni_ _ _ = return ResNone

pluginRoot_ :: a -> b -> c -> d -> Handler Result
pluginRoot_ _ _ _ _ = return ResNone

type Shared = TVar

share :: a -> IO (Shared a)
share = atomically . newTVar

getShare :: MonadIO m => Shared a -> m a
getShare = io . readTVarIO

setShare :: MonadIO m => Shared a -> a -> m ()
setShare var val = io $ atomically $ writeTVar var val

execPluginUni :: Plugin -> IrcMessage -> Handler Result
execPluginUni Plugin{ pluginPersist = p, pluginUni = f } = f p

execPluginRoot :: Plugin -> Text -> Text -> Maybe IrcMessage -> Handler Result
execPluginRoot Plugin{ pluginPersist = p, pluginRoot = f } = f p

-- | execute the quit action on plugins
execPluginQuit :: Plugin -> Handler ()
execPluginQuit _ = return () -- TODO

execPluginRoot' :: Text -> Text -> Text -> Maybe IrcMessage -> Handler Result
execPluginRoot' key cmd args msg = do
    mplugin <- asksPersist (Map.lookup key . persistPlugins)
    case mplugin of
      Just plugin -> execPluginRoot plugin cmd args msg
      Nothing     -> return ResNone

-- * Handler functions

getPersist :: Handler Persist
getPersist = Handler $ lift ask

asksPersist :: (Persist -> a) -> Handler a
asksPersist = Handler . lift . asks

getPlugins :: Handler (Map Text Plugin)
getPlugins = asksPersist persistPlugins

execRoot :: Text     -- ^ command
         -> Text     -- ^ argument
         -> Maybe IrcMessage
         -> [Plugin] -- ^ plugins to try, starting from left-side
         -> Handler Result
execRoot   _    _    _     [] = return ResNone
execRoot cmd args mmsg (p:ps) = execPluginRoot p cmd args mmsg >>= \res -> case res of
    ResNone -> execRoot cmd args mmsg ps
    _       -> return res

execUni :: [Plugin] -> IrcMessage -> Handler Result
execUni     []   _ = return ResNone
execUni (p:ps) msg = execPluginUni p msg >>= \res -> case res of
    ResNone -> execUni ps msg
    _       -> return res

-- | insert a new connection to tvar
insert :: ConData -> Handler ()
insert c = withConnections (\v -> readTVar v >>= writeTVar v . (c :))

-- | execute a Con action with the supplied ConData.
with :: ConData -> Con a -> Handler a
with = (io .) . runCon

withConnections :: (Connections -> STM a) -> Handler a
withConnections f = io =<< asks (atomically . f . snd)

consMap :: Con a -> Handler ()
consMap f = withConnections (readTVar) >>= mapM_ (flip with f)
