{-# LANGUAGE RankNTypes #-}
------------------------------------------------------------------------------
-- File:          Haikubot/Actions.hs
-- Creation Date: Dec 29 2012 [23:59:51]
-- Last Modified: Oct 08 2013 [20:58:02]
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
--  , endIf
--  , ensure
  ) where

import Data.Monoid ( (<>) )
import Control.Monad
import Data.Text (Text)
import qualified Data.Map as Map

import Haikubot.Core
import Haikubot.Connections
import Haikubot.Messages
import Haikubot.Logging

-- * Send

-- | Write a IRC command to con. Res: stop.
writeCommand :: Command -> Action p Res
writeCommand cmd = getActionCon >>= liftHandler . writeCmd' cmd >> stop

-- | Write a raw line to con. Res: stop.
writeRaw :: Text -> Action p Res
writeRaw text = getActionCon >>= liftHandler . writeLine' text >> stop

-- | @privmsg channel msg@ Writes @msg@ to channel @channel@. Res: stop.
privmsg :: Text -> Text -> Action p Res
privmsg a b = writeCommand (MPrivmsg a b)

-- | Reply text to origin. Res: stop.
reply :: Text -> Action p Res
reply msg = do
    morigin <- maybeOrigin
    case morigin of
        Nothing     -> liftHandler $ logOut msg
        Just origin -> do privmsg origin msg
                          liftHandler . logInfo $ "<reply> " <> msg
    stop

-- * Get

getPlugin :: Text -> Handler (Maybe Plugin)
getPlugin pId = liftM (Map.lookup pId . cPlugins) getConfig

onPlugins :: Maybe Con
          -> Maybe IrcMessage
          -> PluginAction
          -> Handler PluginResult
onPlugins mcon mmsg act_f = do
    (x:xs) <- liftM (Map.elems . cPlugins) getConfig
    go x xs
  where
      go :: Plugin -> [Plugin] -> Handler (Either String Res)
      go (MkPlugin d) []     = doAction d
      go (MkPlugin d) (x:xs) = do
          res <- doAction d
          case res of
            Left err      -> return (Left err)
            Right Nothing -> go x xs
            _             -> return res

      doAction persist = runAction act_f (ActionData persist mcon mmsg)

aget :: (p -> a) -> Action p a
aget f = liftM f getActionData

mget :: (IrcMessage -> a) -> Action p a
mget f = liftM f getActionMessage

maybeOrigin :: Action p (Maybe Text)
maybeOrigin = liftM (join . fmap mOrigin) getActionMessage'

requireOrigin :: Action p Text
requireOrigin = maybeOrigin >>= \x -> case x of
    Nothing -> fail "No origin"
    Just x' -> return x'

(===) :: Eq a => (IrcMessage -> a) -> a -> Action p (Maybe Bool)
f === t = liftM (fmap ((==) t . f)) getActionMessage'
