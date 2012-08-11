------------------------------------------------------------------------------
-- File: Plugins.hs
-- Creation Date: Aug 05 2012 [05:38:41]
-- Last Modified: Aug 09 2012 [22:19:40]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
-- | The plugin interface.
module Plugins
  ( Handler, getPersist
  , onSource    -- ^ Perform on the source
  , Plugin(Plugin, pluginPersist, pluginUni, pluginRoot), pluginUni_, pluginRoot_
  , Con, Shared, share, getShare
  , writeRaw, writeText, write
  , Result(..)
  , IrcMessage(..), Command(..)
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
