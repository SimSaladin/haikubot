------------------------------------------------------------------------------
-- File: Bot.hs
-- Creation Date: Jul 23 2012 [11:10:43]
-- Last Modified: Dec 30 2012 [04:10:36]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

-- | General bot functionalities.
module Bot
  ( mainWithCLI
  , quit

  -- * 
  , spawn, connect, disconnect, listen
  , log, logRes, raise

  , module Utils
  , module Internal.Types
  , module Internal.Actions
  , module Internal.Connections
  , module Internal.Messages
  ) where

import           Prelude hiding (catch, getLine, putStr, putStrLn, words, log)
import           Data.ByteString (ByteString) -- so we hide the IsString instance
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import           Network
import           Control.Arrow ((***))
import           Control.Exception hiding (Handler)
import           Control.Monad.Reader
import           Control.Concurrent
import           Text.Printf
import           System.Environment (getArgs)
import           System.Exit
import           System.IO

import Utils
import Internal.Messages
import Internal.Types
import Internal.Connections
import Internal.Actions
import Internal.Listen
import Internal.Settings
import Internal.CLI

-- | Main entry point.
--
-- Create and execute a new handler using the supplied configuration, read a
-- possible configuration file supplied as a command line argument and
-- start the command-line interface.
mainWithCLI :: Config -> IO ()
mainWithCLI conf = runHandler (readRC >> runCLI) conf

-- | Create a new connection, greet the server and and store the connection for
-- later access.
connect :: ByteString -- ^ nick
        -> ByteString -- ^ user
        -> ByteString -- ^ address
        -> ByteString -- ^ real name
        -> Handle
        -> Handler ConData
connect nick user addr real h = do
    insert c
    io $ runCon c greet
    return c
  where
    c = ConData h "" ""
    greet = do
      writeRaw $ "NICK " `B.append` nick
      writeRaw $ B.concat ["USER ",user," ",user," ",addr," :",real]

-- | interpret text passed for example from the commandline
rootCommand :: Maybe IRC.IrcMessage -> Text -> Handler Result
rootCommand mmsg input = let (cmd, args) = cmdSplit input
                         in getPlugins >>= execRoot cmd args mmsg . Map.elems

sourceRootCommands :: FilePath -> Handler ()
sourceRootCommands file = do
    h <- io $ openFile file ReadMode
    mapM_ (rootCommand Nothing . T.pack) . lines =<< io (hGetContents h)
    io $ hClose h

-- | TODO: use an id!
disconnect :: ConData -> Handler ()
disconnect _ = raise "CRITICAL: disconnect is not implemented"

-- | Begin connection listener in a new thread.
listen :: ConData -> Handler ()
listen c = local (const (Just c) *** id) $ getPlugins >>= forever . parse . Map.elems
  where parse plugins = execUni plugins . IRC.parse =<< with c readRaw

-- | Quits haikubot.
quit :: Handler ()
quit = getPlugins >>= mapM execPluginQuit . Map.elems >> io exitSuccess


-- * Logging / Maintainance

logRes :: Result -> Handler ()
logRes (ResFailure msg) = raise $ encodeUtf8 msg
logRes (ResSuccess msg) = log $ encodeUtf8 msg
logRes ResNone          = raise "No handler found"

log :: B.ByteString -> Handler ()
log = io . B.putStrLn . ("[log] " `B.append`)

raise :: B.ByteString -> Handler ()
raise = io . B.putStrLn . ("[warn] " `B.append`)
