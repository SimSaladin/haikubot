{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, ExistentialQuantification #-}
------------------------------------------------------------------------------
-- File:          Internal.hs
-- Creation Date: Dec 29 2012 [20:19:14]
-- Last Modified: Dec 30 2012 [05:50:24]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Internal.Types
  ( Config(..)
  , Res(..)
  , Handler
  , runHandler
  , runHandler'
  , BotData(..)
  , getBotData
  , getConfig
  , Action
  , runAction
  , runAction'
  , ConId
  , Con(..)
  , Connections

  , HaikuPlugin(..)
  , Plugin(..)
  , Plugins
  , lift
  , liftIO
  ) where

import           Data.Text              (Text)
import qualified Data.Map               as Map
import           Control.Monad
import           Control.Concurrent.STM
import           Control.Monad.Trans.Reader hiding (ask)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Reader.Class (MonadReader, ask)
import           Control.Monad.IO.Class
import           Network                (PortID)
import           System.IO              (Handle)
import Internal.Messages

-- * Handler

data Config = Config
  { cRootPrefix :: Text
  , cPlugins    :: Plugins
  }

data BotData = BotData
  { botConfig      :: Config
  , botConnections :: TVar Connections
  }

-- | Handler monad.
newtype Handler a = Handler { runH :: ReaderT BotData IO a }
  deriving (Monad, MonadIO, MonadReader BotData)

-- | Run a handler with empty connections.
runHandler :: Handler a -> Config -> IO a
runHandler handler conf = do
    cons <- atomically $ newTVar Map.empty
    runHandler' handler $ BotData conf cons

runHandler' :: Handler a -> BotData -> IO a
runHandler' handler bdata = runReaderT ((runH handler)) bdata

getBotData :: Handler BotData
getBotData = ask

getConfig :: Handler Config
getConfig = liftM botConfig ask


-- * Action

-- | The action monad is provided to simplify plugin writing. It's a MaybeT over
-- a ReaderT.
newtype Action p a = Action { runA :: MaybeT (ReaderT (p, Maybe IrcMessage) Handler) a }
  deriving (Monad, MonadIO)

-- | TODO: somehow use MonadTrans or something?
lift :: Handler (Maybe a) -> Action p a
lift handler = Action (MaybeT $ ReaderT (\_ -> handler))

runAction :: Action p a -> p -> Maybe IrcMessage -> Handler (Maybe a)
runAction action persist msg = runReaderT (runMaybeT (runA action)) (persist, msg)

runAction' :: Action () a -> Handler (Maybe a)
runAction' action = runAction action () Nothing


-- * Connections

type ConId = Text

data Con = Con
  { conSocket :: Handle
  , conServer :: String
  , conPort   :: PortID
  , conNick   :: Text
  }

type Connections = Map.Map ConId Con


-- * Plugin

data Res = RSucc [Text]
         | Rfail Text

-- | Plugin interface.
class HaikuPlugin p where

  -- | Handle a root command either from command line or from authorized user.
  handleCommand :: (Text, [Text]) -> Action p Res

  -- | Handle a privmsg
  handlePrivmsg :: Action p Res

data Plugin = forall a. HaikuPlugin a => MkPlugin a

type Plugins = Map.Map Text (TVar Plugin)
