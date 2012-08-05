{-# LANGUAGE GeneralizedNewtypeDeriving, ExistentialQuantification #-}
------------------------------------------------------------------------------
-- File: Handler.hs
-- Creation Date: Aug 05 2012 [05:15:24]
-- Last Modified: Aug 05 2012 [22:21:32]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Handler
  ( Config(..)
  , Handler
  , runHandler
  , Plugin(Plugin)
  , Shared
  , share
  , Persist(..)
  , Connection(..)
  , Result(..)
  , Listener(..)
  , withConnections
  , getPersist
  , io
  ) where

import Data.Text (Text)
import Network.SimpleIRC
import Control.Monad.Reader
import Control.Concurrent.STM
import System.IO (Handle)

-- | Represents a single connection to a network
data Connection = Connection
    { conSocket      :: Handle
    , conNetworkName :: Text
    , conNick        :: Text
    }

-- | Network connections are carried in the Handler monad.
type Connections = TVar [Connection]

-- | Handler provides access to any operation. Use with care!
newtype Handler a = Handler {
    runH :: ReaderT Connections (ReaderT Persist IO) a
  } deriving (Monad, MonadIO, MonadReader Connections)

-- | XXX: wrap in newtype?
type Shared = TVar

share :: a -> IO (Shared a)
share = atomically . newTVar

-- | Plugin monad wraps around Handler and has its own state
data Plugin = forall env. Plugin
  { pluginPersist :: Shared env
  , pluginHandler :: Shared env -> Connection -> IrcMessage -> Handler ()
  }

-- |
runHandler :: Handler a -> Persist -> IO a
runHandler h config = do
    bots <- atomically $ newTVar []
    runReaderT (runReaderT (runH h) bots) config

-- |
class Monad a => Listener a where
    before    :: a ()
    process   :: IrcMessage -> a Result
    after     :: a ()
    before    = return ()
    process _ = return ResNone
    after     = return ()

data Config = Config
  { configPlugins :: [IO Plugin]
  }

-- | Handler state. Consistent among listener threads.
data Persist = Persist
  { persistPlugins :: [Plugin]
  }

-- |
data Result = ResNone | ResSuccess Text | ResFailure Text

withConnections :: (Connections -> STM a) -> Handler a
withConnections f = io =<< asks (atomically . f)

getPersist :: Handler Persist
getPersist = Handler $ lift $ ask

io :: MonadIO m => IO a -> m a
io = liftIO
