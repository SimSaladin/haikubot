------------------------------------------------------------------------------
-- File: Config.hs
-- Creation Date: Aug 05 2012 [06:14:42]
-- Last Modified: Oct 05 2013 [23:59:54]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Haikubot.Main 
  ( mainBare
  , mainWithCLI
  , defaultConfig
  -- * Re-exports
  , Basics(..)
  ) where

import Haikubot
import Haikubot.Settings
import Haikubot.CLI
import Haikubot.Plugins.Runot
import Haikubot.Plugins.Basics

import Data.Map (fromList, empty)
import qualified Data.Map as M
import Control.Concurrent.STM
import Control.Concurrent.MVar
import System.IO.Unsafe


-- | Main entry point.
--
-- Create and execute a new handler using the supplied configuration, read a
-- possible configuration file supplied as a command line argument and
-- start the command-line interface.
mainWithCLI :: Config -> IO ()
mainWithCLI conf = runHandler (readRC >> runCLI) conf

-- | Default configuration.
defaultConfig :: Config
defaultConfig = Config
  { cRootPrefix = "@"
  , cPlugins    = fromList [ ("Basics", MkPlugin Basics)
                           , ("runot" , MkPlugin runot)
                           ]
  }

runot :: Runot
runot = Runot ["#haiku", "#haiku-testing"]
              "/home/tk/haikubot/haikut.txt"
              empty
              (unsafePerformIO $ newTMVarIO empty)

-- | Read configuration file and start without CLI.
mainBare :: Config -> IO ()
mainBare conf = runHandler (readRC >> monitor) conf

-- | TODO: replace with an internal dbus hontrol channel.
monitor :: Handler ()
monitor = do
  botData <- getBotData
  let go = liftIO (takeMVar (botInternalChannel botData) >>= print) >> go
  go
