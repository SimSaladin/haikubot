{-# LANGUAGE RankNTypes #-}
------------------------------------------------------------------------------
-- File:          Internal/Actions.hs
-- Creation Date: Dec 29 2012 [23:59:51]
-- Last Modified: Dec 30 2012 [06:21:57]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
-- | General actions.
module Internal.Actions
  ( readLine
  , writeLine
  , writeCmd
  , getPlugin
  , onPlugins
  ) where

import Control.Monad
import Data.Text (Text)
import Data.Maybe (catMaybes)
import Control.Concurrent.STM
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import System.IO (Handle)

import Internal.Types
import Internal.Connections
import Internal.Messages

getHandle :: ConId -> Action a Handle
getHandle = liftM conSocket . lift . getCon


-- * Con

readLine :: ConId -> Action a Text
readLine conId = getHandle conId >>= liftIO . T.hGetLine

-- | Write a raw text to a Con.
writeLine :: Text -> ConId -> Action a ()
writeLine t conId = getHandle conId >>= liftIO . flip T.hPutStrLn t

writeCmd :: Command -> ConId -> Action a ()
writeCmd cmd conId = writeLine (showCommand cmd) conId


-- * Plugins

getPlugin :: Text -> Handler (Maybe Plugin)
getPlugin pId = do
    ps <- liftM cPlugins getConfig
    case Map.lookup pId ps of
      Nothing -> return Nothing
      Just tvar -> liftM Just $ liftIO $ readTVarIO tvar

onPlugins :: Maybe IrcMessage -> (forall p. HaikuPlugin p => Action p r) -> Handler [r]
onPlugins mmsg f = liftM (Map.elems . cPlugins) getConfig
    >>= liftM catMaybes . mapM (\tvar -> liftIO (readTVarIO tvar) >>= f')
  where
    f' (MkPlugin persist) = runAction f persist mmsg
