------------------------------------------------------------------------------
-- File:          Haikubot/Basics.hs
-- Creation Date: Dec 31 2012 [09:16:40]
-- Last Modified: Dec 31 2012 [09:51:07]
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
    handleCmd ("join"   , [chan]     ) = writeCommand $ MJoin chan Nothing
    handleCmd ("join"   , [chan, key]) = writeCommand $ MJoin chan (Just key)
    handleCmd ("rawirc" , xs         ) = writeRaw $ T.intercalate " " xs
    handleCmd ("quit"   , _          ) = lift $ liftM Just exit
    handleCmd ("connect", conId:server:port:nick:user:real) = do
        res <- lift . liftM Just $ makeConnection conId (T.unpack server) port' exec
        case res of
            Left err -> lift . liftM Just $ logErr err
            Right _  -> reply $ "Connection to " `mappend` conId `mappend` " established."
      where
        port' = PortNumber $ fromIntegral (read $ T.unpack port :: Integer)
        real' = T.intercalate " " real
        exec con = greet con nick user real' >> listener con

    handleCmd ("connect", _) = reply "Syntax: connect <identifier> <server> <port> <nick> <user> <realname>"
    handleCmd (_        , _) = return ()

    handlePrivmsg = do
        code <- mget mCode
        case code of
          "PING"    -> mget mMsg >>= writeRaw . T.append "PONG :"
          "PRIVMSG" -> do
              prefix <- lift $ liftM (Just . cRootPrefix) getConfig
              msg    <- mget mMsg
              guard $ prefix `T.isPrefixOf` msg
              amsg <- getActionMessage'
              acon <- getActionCon'
              lift $ liftM Just $ runCmd amsg acon msg
          _ -> return ()

listener :: Con -> Handler ()
listener con = forever $ do
    line <- readLine' con
    logInfo line
    runMsg (Just con) (Just $ parse line)
    return ()

greet :: Con
      -> Text -- ^ nick
      -> Text -- ^ user
      -> Text -- ^ real name
      -> Handler ()
greet con nick user real = do
    writeCmd' (MNick nick) con
    writeCmd' (MUser user "0" real) con
