------------------------------------------------------------------------------
-- File:          Haikubot/Basics.hs
-- Creation Date: Dec 31 2012 [09:16:40]
-- Last Modified: Oct 08 2013 [22:35:24]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
-- | Basic commands and actions.
module Haikubot.Plugins.Basics (Basics(..)) where

import           Haikubot
import           Haikubot.Commands
import qualified Data.Text as T
import           Network (PortID(PortNumber))

data Basics = Basics

instance HaikuPlugin Basics where
    handleCmd ("echo"   , xs         ) = reply $ T.intercalate " " xs

    handleCmd ("join"   , [chan]       ) = writeCommand $ MJoin chan Nothing
    handleCmd ("join"   , [chan, conId]) = writeCommandTo (MJoin chan Nothing) conId
    handleCmd ("join"   , _            ) = reply "syntax: join <channel> [<conId>]"

    handleCmd ("rawirc" , xs         ) = writeRaw $ T.intercalate " " xs
    handleCmd ("quit"   , _          ) = liftHandler exit >> stop
    handleCmd ("connect", conId:server:port:nick:user:real) = connect conId server port nick user (T.intercalate " " real)
    handleCmd ("connect", _) = reply "Syntax: connect <identifier> <server> <port> <nick> <user> <realname>"
    handleCmd (_        , _) = noop

    handleIrcMessage msg
        | mCode msg == "PING"    = writeRaw ("PONG :" <> mMsg msg) >> stop
        | mCode msg == "PRIVMSG" = do
            config <- liftHandler getConfig
            maybe noop runConCmd (cRootPrefix config `T.stripPrefix` mMsg msg)
        | otherwise = noop
            where
                runConCmd cmd = do
                    con <- getActionCon'
                    liftHandler $ runCmd con (Just msg) cmd
                    stop

-- | Connect to the server, tell our nick and user and start the listen
-- loop in a new thread.
connect :: Text -- ^ connection id
        -> Text -- ^ server domain
        -> Text -- ^ Server port
        -> Text -- ^ Nick
        -> Text -- ^ User name
        -> Text -- ^ Real name
        -> Action Basics Res
connect conId server port nick user real = do
    res <- liftHandler $ makeConnection conId (T.unpack server) port' exec
    case res of
        Left err -> liftHandler (logErr err) >> stop
        Right _  -> reply $ "Connection to " <> conId <> " established."
  where
    port' = PortNumber $ fromIntegral (read $ T.unpack port :: Integer)
    exec con = greet con nick user real >> listener con

greet :: Con
      -> Text -- ^ nick
      -> Text -- ^ user
      -> Text -- ^ real name
      -> Handler ()
greet con nick user real = do
    writeCmd' (MNick nick) con
    writeCmd' (MUser user "0" real) con

listener :: Con -> Handler ()
listener con = forever $ do
    line <- readLine' con
    let cmd = parse line
    unless (mCode cmd == "PING") $ logInfo line
    runMsg con cmd
