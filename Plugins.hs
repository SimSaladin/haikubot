{-# LANGUAGE GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
-- File: Plugins.hs
-- Creation Date: Aug 05 2012 [05:38:41]
-- Last Modified: Aug 06 2012 [06:29:17]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
-- | The plugin interface.
module Plugins
  ( Handler, getPersist
  , Plugin(Plugin, pluginPersist, pluginUni, pluginRoot), pluginUni_, pluginRoot_, Con, Shared, share
  , writeRaw, write
  , Result(..)
  , IrcMessage(..)
  , Text, encodeUtf8, decodeUtf8
  , none, failed, success
  ) where

import Network.SimpleIRC
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Connections
import Handler

none :: Monad m => m Result
none = return ResNone

failed :: Monad m => Text -> m Result
failed = return . ResFailure

success :: Monad m => Text -> m Result
success = return . ResSuccess
