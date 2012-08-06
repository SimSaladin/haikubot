{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- File: Fundamentals.hs
-- Creation Date: Aug 06 2012 [03:16:19]
-- Last Modified: Aug 06 2012 [06:32:36]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
-- | Fundamental functionalities for haikubot.
module Fundamentals where

import Utils
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Plugins
import Haiku (spawn, connect, disconnect, listen, quit)

boot :: IO Plugin
boot = do
  env <- share (Env True)
  return $ Plugin { pluginPersist = env
                  , pluginUni     = handleUni
                  , pluginRoot    = handleRoot 
                  }

data Env = Env { something :: Bool }

handleUni :: Shared Env -> IrcMessage -> Con Result
handleUni _ msg = case mCode msg of
  "PING" -> let resp = "PONG :" `B.append` (mMsg msg)
                in writeRaw resp >> success (decodeUtf8 resp)
  _      -> none

handleRoot :: Shared Env -> Text -> Text -> Handler Result
handleRoot   _ "help" arg
  | arg ==       ""  = failed "haikubot: global help is not implemented"
  | arg == "connect" = success "usage: connect <server> <port> <nick> <user> <addr> <{realname}>"
  | otherwise        = none

handleRoot _ "quit" _ = quit >> failed "hm.. I'm supposed to be dead by now.."

handleRoot env cmd arg
  | cmd == "connect"    = parseConnect arg
  | cmd == "disconnect" = failed "no disconnect yet"
  | otherwise = none

parseConnect :: Text -> Handler Result
parseConnect input
  | length params >= 6 = do
    persist <- getPersist
    tid <- io $ spawn persist server port connect' listen disconnect
    success $ T.pack $ show tid
  | otherwise = failed "connect: invalid amount of arguments"
  where
    params   = T.words input
    server   = T.unpack (params !! 0)
    port     = read $ T.unpack (params !! 1)
    connect' = connect (encodeUtf8 $ params !! 2)
                       (encodeUtf8 $ params !! 3)
                       (encodeUtf8 $ params !! 4)
                       (encodeUtf8 $ T.concat $ drop 5 params)

