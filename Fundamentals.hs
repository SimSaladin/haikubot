------------------------------------------------------------------------------
-- File: Fundamentals.hs
-- Creation Date: Aug 06 2012 [03:16:19]
-- Last Modified: Aug 19 2012 [11:03:00]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
-- | Fundamental functionalities for haikubot.
module Fundamentals where

import Handler (getPlugins, execRoot, execPluginRoot')
import Utils
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Map as Map
import Plugins
import Haiku (spawn, connect, disconnect, listen, quit)

boot :: IO (Text, Plugin)
boot = do
  env <- share (Env True)
  return $ (,) "Fund" $ Plugin { pluginPersist = env
                  , pluginUni     = handleUni
                  , pluginRoot    = handleRoot 
                  }

data Env = Env { something :: Bool }

handleUni :: Shared Env -> IrcMessage -> Handler Result
handleUni _ msg@IrcMessage{ mCode = mcode, mMsg = mmsg}
  | mcode == "PING" = let resp = "PONG :" `B.append` mmsg
                in replyRaw resp >> success (decodeUtf8 resp)
  | mcode == "PRIVMSG" = do
      prefix <- asksPersist persistRootPrefix
      let (cmd, args) = cmdSplit $ decodeUtf8 $ B.drop (B.length prefix) mmsg
      if prefix `B.isPrefixOf` mmsg
        then execRoot cmd args (Just msg) . Map.elems =<< getPlugins
        else none
  | otherwise = none

handleRoot :: Shared Env -> Text -> Text -> Maybe IrcMessage -> Handler Result
handleRoot _ cmd arg mmsg
  | cmd == "quit"       = quit >> failed "hm.. I'm supposed to be dead by now.."
  | cmd == "connect"    = parseConnect arg
  | cmd == "disconnect" = failed "no disconnect yet implemented"
  | cmd == "rawirc"     = replyText arg
  | cmd == "rawircall"  = consMap (writeText arg) >> success "wrote to all connections"
  -- TODO: write to specific Con
  | cmd == "join"       = parseJoin arg
  | cmd == "help"       = case arg of
    ""        -> failed "haikubot: global help is not implemented"
    "connect" -> success "usage: connect <server> <port> <nick> <user> <addr> <{realname}>"
    _         -> none
  | otherwise           = none
  where
    isAuthorized = case mmsg of
      Nothing -> return True
      Just IrcMessage{ mNick = mnick } -> case mnick of
        Nothing   -> return False
        Just nick -> do
            res <- execPluginRoot' "AccessControl" "isadmin" (decodeUtf8 nick) mmsg
            case res of
              ResFailure _ -> return False
              _            -> return True

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

parseJoin :: Text -> Handler Result
parseJoin arg = let
  join c k = reply (MJoin (encodeUtf8 c) (fmap encodeUtf8 k))
  in case (T.words arg) of
    (chan:[])     -> join chan Nothing
    (chan:key:[]) -> join chan (Just key)
    _             -> failed "Invalid amount of arguments"
