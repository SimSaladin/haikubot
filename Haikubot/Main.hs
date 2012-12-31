------------------------------------------------------------------------------
-- File: Config.hs
-- Creation Date: Aug 05 2012 [06:14:42]
-- Last Modified: Dec 31 2012 [14:33:26]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Haikubot.Main 
  ( mainWithCLI
  , defaultConfig
  -- * Re-exports
  , Basics(..)
  ) where

import Data.Map (fromList, empty)

import Haikubot
import Haikubot.Settings
import Haikubot.CLI
import Haikubot.Plugins.Runot
import Haikubot.Plugins.Basics

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
              "/home/sim/docs/haikut.txt"
              empty
