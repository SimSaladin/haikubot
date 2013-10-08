{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, ExistentialQuantification #-}
------------------------------------------------------------------------------
-- File:          Haikubot.hs
-- Creation Date: Dec 29 2012 [20:19:14]
-- Last Modified: Oct 08 2013 [20:44:40]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Haikubot.Core
  ( -- * Data Types
    Config(..)
  , BotData(..)
  , ActionData(..)

  -- ** Connection types
  , Con(..)
  , ConId
  , Connections

  -- ** Plugin types
  , HaikuPlugin(..)
  , PluginAction, PluginResult
  , Plugin(..)
  , Plugins

  -- ** Event results
  , Res
  , noop, stop

  -- * The Monads
  , Handler
  , Action
  , runHandler
  , runHandler'
  , runAction

  -- * Helpers
  -- ** Handler helpers
  , getBotData
  , getConfig

  -- ** Action helpers
  , lift
  , liftHandler
  , liftIO
  , getActionData
--   , putActionData
--   , modifyActionData
  , getActionCon
  , getActionMessage
  , getActionCon'
  , getActionMessage'
  ) where

import           Haikubot.Messages

import           Data.Text                      (Text)
import qualified Data.Map               as      Map
import           Control.Applicative
import           Control.Monad
import           Control.Concurrent.STM
import           Network                        (PortID)
import           System.IO                      (Handle)
import           Control.Monad.Error     hiding (lift)
import           Control.Monad.Reader    hiding (lift)

-- | Settings for Haikubot.
data Config = Config
  { cRootPrefix :: Text    -- ^ Prefix to use for root (admin) commands.
  , cPlugins    :: Plugins -- ^ Plugins to use.
  }

-- | Internal data shared by threads.
data BotData = BotData
  { botConfig      :: TVar Config
  , botConnections :: TVar Connections
  }

-- | Additional data available n plugins' functions.
data ActionData p = ActionData 
    { actionPersist :: p
    , actionCon     :: Maybe Con
    , actionMessage :: Maybe IrcMessage
    }

-- | A connection to a server.
data Con = Con
  { conSocket :: Handle
  , conServer :: String
  , conPort   :: PortID
  , conNick   :: Text
  }

-- | Connections are identified by ConIds (Texts).
type ConId = Text

-- | Connections used by the bot.
type Connections = Map.Map ConId Con

-- | Results from plugin handlers. On Nothing, continue in event stack,
-- otherwise stop. Left values are errors.
type Res = Maybe ()

noop, stop :: Action p Res
noop = return Nothing
stop = return (Just ())

type PluginAction = forall p. HaikuPlugin p => Action p Res
type PluginResult = Either String Res

-- | Plugin interface.
class HaikuPlugin p where

  -- | Handle a root command either from command line or from authorized user.
  handleCmd :: (Text, [Text]) -> Action p Res
  handleCmd _ = fail "No action"

  -- | Handle a privmsg.
  handleIrcMessage :: IrcMessage -> Action p Res
  handleIrcMessage _ = noop

  -- | Action to run before haikubot exit.
  onExit :: Action p ()
  onExit = return ()

data Plugin = forall a. HaikuPlugin a => MkPlugin a

type Plugins = Map.Map Text Plugin


-- | Handler monad.
newtype Handler a = Handler { runH :: ReaderT BotData IO a }
  deriving (Functor, Monad, MonadIO, MonadReader BotData)

-- | The action monad is provided to simplify plugin writing. It's a MaybeT over
-- a ReaderT.
newtype Action p a = Action { runA :: ErrorT String (ReaderT (ActionData p) Handler) a }
  deriving (Functor, Monad, MonadIO, MonadPlus, MonadReader (ActionData p), MonadError String)

-- | Run a handler with empty connections.
runHandler :: Handler a -> Config -> IO a
runHandler handler conf = do
    cons  <- atomically $ newTVar Map.empty
    conf' <- atomically $ newTVar conf
    runHandler' handler $ BotData conf' cons

runHandler' :: Handler a -> BotData -> IO a
runHandler' handler = runReaderT (runH handler)

runAction :: Action p a -> ActionData p -> Handler (Either String a)
runAction action = runReaderT (runErrorT (runA action))
-- runAction' action = runAction action $ ActionData () Nothing Nothing


getBotData :: Handler BotData
getBotData = ask

getConfig :: Handler Config
getConfig = liftM botConfig ask >>= liftIO . readTVarIO

getActionData :: Action plugdata plugdata
getActionData = liftM actionPersist ask

-- putActionData :: p -> Action p ()
-- modifyActionData :: (p -> p) -> Action p ()
-- putActionData x = modify (\y -> y{actionPersist = x})
-- modifyActionData f = modify (\p@ActionData{actionPersist = x} -> p{actionPersist = f x})

getActionCon' :: Action p (Maybe Con)
getActionCon' = liftM actionCon ask

getActionCon :: Action p Con
getActionCon = getActionCon' >>= \x -> case x of
    Nothing -> fail "No connection available."
    Just con -> return con

getActionMessage' :: Action p (Maybe IrcMessage)
getActionMessage' = liftM actionMessage ask

getActionMessage :: Action p IrcMessage
getActionMessage = getActionMessage' >>= \x -> case x of
    Nothing  -> fail "No message."
    Just msg -> return msg

-- | TODO: somehow use MonadTrans or something?
lift :: Handler (Maybe a) -> Action p a
lift handler = Action $ ErrorT $ ReaderT (\_ -> fmap (maybe (Left "") Right) handler)

liftHandler :: Handler a -> Action p a
liftHandler handler = lift (Just <$> handler)

