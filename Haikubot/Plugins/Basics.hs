------------------------------------------------------------------------------
-- File:          Haikubot/Basics.hs
-- Creation Date: Dec 31 2012 [09:16:40]
-- Last Modified: Oct 08 2013 [21:08:56]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
-- | Basic commands and actions.
module Haikubot.Plugins.Basics (Basics(..)) where

import           Haikubot
import           Haikubot.Commands
import           Data.Monoid
import qualified Data.Text as T
import           Network (PortID(PortNumber))

data Basics = Basics

instance HaikuPlugin Basics where
    handleCmd ("echo"   , xs         ) = reply $ T.intercalate " " xs
    handleCmd ("join"   , [chan]     ) = writeCommand $ MJoin chan Nothing
    handleCmd ("join"   , [chan, key]) = writeCommand $ MJoin chan (Just key)
    handleCmd ("rawirc" , xs         ) = writeRaw $ T.intercalate " " xs
    handleCmd ("quit"   , _          ) = liftHandler exit >> stop
    handleCmd ("connect", conId:server:port:nick:user:real) = do
        res <- liftHandler $ makeConnection conId (T.unpack server) port' exec
        case res of
            Left err -> liftHandler (logErr err) >> stop
            Right _  -> reply $ "Connection to " <> conId <> " established."
      where
        port' = PortNumber $ fromIntegral (read $ T.unpack port :: Integer)
        real' = T.intercalate " " real
        exec con = greet con nick user real' >> listener con

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

listener :: Con -> Handler ()
listener con = forever $ do
    line <- readLine' con
    logInfo line
    runMsg con (parse line)
    return ()

greet :: Con
      -> Text -- ^ nick
      -> Text -- ^ user
      -> Text -- ^ real name
      -> Handler ()
greet con nick user real = do
    writeCmd' (MNick nick) con
    writeCmd' (MUser user "0" real) con

