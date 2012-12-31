------------------------------------------------------------------------------
-- File: Bot.hs
-- Creation Date: Jul 23 2012 [11:10:43]
-- Last Modified: Dec 31 2012 [07:15:13]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

-- | General bot functionalities.
module Bot
  ( mainWithCLI
  , exit
  , Basics(..)
  , module Utils
  , module Internal.Types
  , module Internal.Actions
  , module Internal.Connections
  , module Internal.Messages
  , Text, liftM
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid (mappend)
import           Network
import           Control.Monad (liftM, guard, forever)
import           System.Exit
import Utils
import Logging
import Internal.Actions
import Internal.CLI
import Internal.Commands
import Internal.Connections
import Internal.Messages
import Internal.Settings
import Internal.Types

-- | Main entry point.
--
-- Create and execute a new handler using the supplied configuration, read a
-- possible configuration file supplied as a command line argument and
-- start the command-line interface.
mainWithCLI :: Config -> IO ()
mainWithCLI conf = runHandler (readRC >> runCLI) conf

-- | Exit haikubot. Call the exit handler on plugins first.
exit :: Handler ()
exit = do
    _ <- onPlugins Nothing Nothing onExit
    liftIO exitSuccess

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

-- * Basic functionality

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

