{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification, OverloadedStrings #-}
------------------------------------------------------------------------------
-- File: Handler.hs
-- Creation Date: Aug 05 2012 [05:15:24]
-- Last Modified: Aug 09 2012 [22:19:42]
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
  , pluginUni_
  , pluginRoot_

  -- * Interaction
  , execPluginUni
  , execPluginRoot
  , execPluginQuit
  , execRoot
  , execUni

  -- * Handler functions
  , with
  , insert
  , withConnections
  , getPersist
  , getPlugins
  , onSource
  ) where

import Prelude hiding (log)
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
    runH :: ReaderT Connections (ReaderT Persist IO) a
  } deriving (Monad, MonadIO, MonadReader Connections)

-- |
runHandler :: Handler a -> Persist -> IO a
runHandler h config = do
    cons <- atomically $ newTVar []
    runReaderT (runReaderT (runH h) cons) config

data Config = Config
  { cRootPrefix :: Text
  , cPlugins    :: [IO Plugin]
  }

-- | 
data Persist = Persist { persistPlugins :: [Plugin] }

-- |
data Result = ResNone | ResSuccess Text | ResFailure Text


-- * Plugin stuff

-- | XXX: Use some typeclass instead?
data Plugin = forall env. Plugin
  { pluginPersist :: Shared env
  , pluginUni     :: Shared env -> IrcMessage -> Con Result
  , pluginRoot    :: Shared env -> Text -> Text -> Handler Result
--, pluginAny     :: Shared env -> IrcMessage -> Handler Result
  }

pluginUni_ :: a -> b -> Con Result
pluginUni_ _ _ = return ResNone

pluginRoot_ :: a -> b -> c -> Handler Result
pluginRoot_ _ _ _ = return ResNone

type Shared = TVar

share :: a -> IO (Shared a)
share = atomically . newTVar

getShare :: MonadIO m => Shared a -> m a
getShare = io . readTVarIO

execPluginUni :: Plugin -> IrcMessage -> Con Result
execPluginUni Plugin{ pluginPersist = p, pluginUni = f } = f p

execPluginRoot :: Plugin -> Text -> Text -> Handler Result
execPluginRoot Plugin{ pluginPersist = p, pluginRoot = f } = f p

execPluginQuit :: Plugin -> Handler ()
execPluginQuit Plugin{ pluginPersist = p, pluginUni = f } = return () -- TODO


-- * Handler functions

getPersist :: Handler Persist
getPersist = Handler $ lift ask

onSource :: Con a -> Handler a
onSource action = undefined -- TODO

getPlugins :: Handler [Plugin]
getPlugins = Handler $ lift $ asks persistPlugins

execRoot :: Text     -- ^ command
         -> Text     -- ^ argument
         -> [Plugin] -- ^ plugins to try, starting from left-side
         -> Handler Result
execRoot   _    _     [] = return ResNone
execRoot cmd args (p:ps) = execPluginRoot p cmd args >>= \res -> case res of
    ResNone -> execRoot cmd args ps
    _       -> return res

execUni :: IrcMessage -> [Plugin] -> Con Result
execUni   _     [] = return ResNone
execUni msg (p:ps) = execPluginUni p msg >>= \res -> case res of
    ResNone -> execUni msg ps
    _       -> return res

insert :: ConData -> Handler ()
insert c = withConnections (\v -> readTVar v >>= writeTVar v . (c :))

with :: ConData -> Con a -> Handler a
with = (io .) . runCon

withConnections :: (Connections -> STM a) -> Handler a
withConnections f = io =<< asks (atomically . f)

