{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, ExistentialQuantification #-}
------------------------------------------------------------------------------
-- File:          Haikubot.hs
-- Creation Date: Dec 29 2012 [20:19:14]
-- Last Modified: Dec 31 2012 [14:32:17]
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
  , Plugin(..)
  , Plugins
  , Res(..)

  -- * The Monads
  , Handler
  , Action
  , runHandler
  , runHandler'
  , runAction
  , runAction'

  -- * Helpers
  -- ** Handler helpers
  , getBotData
  , getConfig

  -- ** Action helpers
  , lift
  , end
  , getActionData
  , getActionCon
  , getActionMessage
  , getActionCon'
  , getActionMessage'
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
import           Haikubot.Messages

data Config = Config
  { cRootPrefix :: Text
  , cPlugins    :: Plugins
  }

data BotData = BotData
  { botConfig      :: TVar Config
  , botConnections :: TVar Connections
  }

data ActionData p = ActionData 
    { actionPersist :: p
    , actionCon     :: Maybe Con
    , actionMessage :: Maybe IrcMessage
    }


type ConId = Text

data Con = Con
  { conSocket :: Handle
  , conServer :: String
  , conPort   :: PortID
  , conNick   :: Text
  }

type Connections = Map.Map ConId Con

data Res = RSucc [Text]
         | Rfail Text


-- | Plugin interface.
class HaikuPlugin p where

  -- | Handle a root command either from command line or from authorized user.
  handleCmd :: (Text, [Text]) -> Action p ()
  handleCmd _ = fail "No action"

  -- | Handle a privmsg.
  handlePrivmsg :: Action p ()
  handlePrivmsg = fail "No action"

  -- | Action to run before haikubot exit.
  onExit :: Action p ()
  onExit = return ()

data Plugin = forall a. HaikuPlugin a => MkPlugin a

type Plugins = Map.Map Text (Plugin)


-- | Handler monad.
newtype Handler a = Handler { runH :: ReaderT BotData IO a }
  deriving (Functor, Monad, MonadIO, MonadReader BotData)

-- | The action monad is provided to simplify plugin writing. It's a MaybeT over
-- a ReaderT.
newtype Action p a = Action { runA :: MaybeT (ReaderT (ActionData p) Handler) a }
  deriving (Monad, MonadIO, MonadPlus, MonadReader (ActionData p))

-- | Run a handler with empty connections.
runHandler :: Handler a -> Config -> IO a
runHandler handler conf = do
    cons  <- atomically $ newTVar Map.empty
    conf' <- atomically $ newTVar conf
    runHandler' handler $ BotData conf' cons

runHandler' :: Handler a -> BotData -> IO a
runHandler' handler bdata = runReaderT (runH handler) bdata

runAction :: Action p a -> ActionData p -> Handler (Maybe a)
runAction action adata = runReaderT (runMaybeT (runA action)) adata

-- | Run an action with "empty" ActionData
runAction' :: Action () a -> Handler (Maybe a)
runAction' action = runAction action $ ActionData () Nothing Nothing


getBotData :: Handler BotData
getBotData = ask

getConfig :: Handler Config
getConfig = liftM botConfig ask >>= liftIO . readTVarIO

getActionData :: Action p p
getActionData = liftM actionPersist ask

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
lift handler = Action (MaybeT $ ReaderT (\_ -> handler))

end :: Action p ()
end = fail "No operation."

