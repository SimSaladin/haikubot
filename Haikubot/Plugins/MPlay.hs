------------------------------------------------------------------------------
-- File:          Plugins/MPlay.hs
-- Creation Date: Aug 05 2012 [05:37:06]
-- Last Modified: Aug 19 2012 [11:03:20]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
-- | This plugin provides IRC interface to MPlayer and rnfssp.
-- Provided commands:
--  * `status`
--  * `toggle`
--  * `play [<playlist>]`
--  * `stop`
--    Acccepts either playlists or 
module Plugins.MPlay where

import Data.Text as T
import Plugins
import Utils

boot :: IO (Text, Plugin)
boot = do
    env <- share $ Env "/usr/bin/mplayer" ["-vo xv"] ""
    return $ (,) "MPlay" $ Plugin { pluginPersist = env
                    , pluginUni     = handleUni
                    , pluginRoot    = pluginRoot_ }

data Env = Env { eMplayer     :: FilePath
               , eMplayerArgs :: [String]
               , eStatus      :: String
               }

handleUni :: Shared Env -> IrcMessage -> Handler Result
handleUni env message
  | mcode == "PRIVMSG" && prefixed = handleCommand message
  | otherwise                      = none
  where mmsg     = decodeUtf8 $ mMsg message
        mcode    = mCode message
        prefixed = "\\" `isPrefixOf` mmsg

handleCommand :: IrcMessage -> Handler Result
handleCommand IrcMessage{mMsg = msg, mOrigin = origin} = case origin of
  Nothing   -> none
  Just dest -> case cmd of
    "status"  -> reply (MPrivmsg dest "status N/A")  >> failed "fimxe:status"
    "play"    -> reply (MPrivmsg dest "play NYI")    >> failed "fixme:play"
    "stop"    -> reply (MPrivmsg dest "stop NYI")    >> failed "fixme:stop"
    "volup"   -> reply (MPrivmsg dest "volup NYI")   >> failed "fixme:volup"
    "voldown" -> reply (MPrivmsg dest "voldown NYI") >> failed "fixme:voldown"
    _         -> none
  where (cmd, args) = cmdSplit $ T.drop 1 $ decodeUtf8 msg

