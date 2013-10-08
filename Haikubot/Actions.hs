{-# LANGUAGE RankNTypes #-}
------------------------------------------------------------------------------
-- File:          Haikubot/Actions.hs
-- Creation Date: Dec 29 2012 [23:59:51]
-- Last Modified: Oct 06 2013 [13:35:08]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
-- | General actions.
module Haikubot.Actions
  ( -- * Plugins
    getPlugin
  , onPlugins

  -- * Actions
  -- ** send
  , writeCommand
  , writeRaw
  , reply

  -- ** plugin state
  , mget
  , aget

  , privmsg

  -- ** User identification
  , maybeOrigin
  , requireOrigin

  -- ** 'checks'
  , (===)
  , endIf
  , ensure
  ) where

import Data.Monoid (mappend)
import Control.Monad
import Data.Text (Text)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map

import Haikubot.Core
import Haikubot.Connections
import Haikubot.Messages
import Haikubot.Logging


writeCommand :: Command -> Action p ()
writeCommand cmd = getActionCon >>= lift . liftM Just . writeCmd' cmd

writeRaw :: Text -> Action p ()
writeRaw text = getActionCon >>= lift . liftM Just . writeLine' text


getPlugin :: Text -> Handler (Maybe Plugin)
getPlugin pId = do
    liftM (Map.lookup pId . cPlugins) getConfig

type PluginAction a = forall p. HaikuPlugin p => Action p a

-- | Execute some action on all plugins
onPlugins :: Maybe IrcMessage -> Maybe Con
          -> PluginAction r -> Handler [Either String r]
onPlugins mmsg mcon f = liftM (Map.elems . cPlugins) getConfig
    >>= mapM f'
  where
    f' (MkPlugin persist) = runAction f (ActionData persist mcon mmsg)


aget :: (p -> a) -> Action p a
aget f = liftM f getActionData

mget :: (IrcMessage -> a) -> Action p a
mget f = liftM f getActionMessage

privmsg :: Text -> Text -> Action p ()
privmsg a b = writeCommand $ MPrivmsg a b

maybeOrigin :: Action p (Maybe Text)
maybeOrigin = liftM (join . fmap mOrigin) getActionMessage'

requireOrigin :: Action p Text
requireOrigin = maybeOrigin >>= \x -> case x of
    Nothing -> fail "No origin"
    Just x' -> return x'

reply :: Text -> Action p ()
reply msg = maybeOrigin >>= \x -> case x of
    Nothing     -> lift $ liftM Just $ logOut msg
    Just origin -> do
        privmsg origin msg
        lift $ liftM Just $ logInfo $ "<reply> " `mappend` msg

(===) :: Eq a => (IrcMessage -> a) -> a -> Action p (Maybe Bool)
f === t = liftM (fmap ((==) t . f)) getActionMessage'

endIf :: Action p (Maybe Bool) -> Action p ()
endIf a = a >>= \x -> case x of
    Just True -> end
    _         -> return ()

ensure :: Action p (Maybe Bool) -> Action p ()
ensure a = a >>= \x -> case x of
    Just True -> return ()
    _         -> end
