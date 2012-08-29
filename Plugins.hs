------------------------------------------------------------------------------
-- File: Plugins.hs
-- Creation Date: Aug 05 2012 [05:38:41]
-- Last Modified: Aug 19 2012 [11:34:06]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
-- | The plugin interface.
module Plugins
  ( Handler, getPersist, asksPersist, Persist(..)
  , Plugin(Plugin, pluginPersist, pluginUni, pluginRoot), pluginUni_, pluginRoot_
  , Con, Shared, share, getShare, setShare, consMap
  , reply, replyRaw, replyText
  , write, writeRaw, writeText
  , Result(..)
  , IrcMessage(..), Command(..)
  , Text, encodeUtf8, decodeUtf8
  , none, failed, success

  , asks
  ) where

import Network.SimpleIRC
import Control.Monad.Reader (asks)
import Data.ByteString (ByteString)
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

reply :: Command -> Handler Result
reply = replyRaw . showCommand

replyText :: Text -> Handler Result
replyText = replyRaw . encodeUtf8

replyRaw :: ByteString -> Handler Result
replyRaw = maybeCon . writeRaw

maybeCon :: Con a -> Handler Result
maybeCon act = asks fst >>= \mcon -> case mcon of
  Just con -> with con act >> success ""
  Nothing  -> failed "no connection (origin not specified)"
