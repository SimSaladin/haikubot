------------------------------------------------------------------------------
-- File: Config.hs
-- Creation Date: Aug 05 2012 [06:14:42]
-- Last Modified: Dec 31 2012 [04:22:01]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Config 
  (defaultConfig) where

import Bot
import Data.Map (fromList)

-- Import plugins here
-- import qualified Plugins.Runot as Runot

-- | Default configuration.
defaultConfig :: Config
defaultConfig = Config
  { cRootPrefix = "@"
  , cPlugins    = fromList [ ("Basics", MkPlugin Basics) ]
  }
