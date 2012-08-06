------------------------------------------------------------------------------
-- File: Config.hs
-- Creation Date: Aug 05 2012 [06:14:42]
-- Last Modified: Aug 06 2012 [05:27:07]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Config 
  ( defaultConfig
  , basicConfig
  , Persist(..)
  ) where

import Handler
import qualified Fundamentals as Fund
import qualified Plugins.MPlay as MPlay

basicConfig :: Config
basicConfig = defaultConfig
  { cPlugins = [ Fund.boot, MPlay.boot ] }

-- | default configuration, without plugins
defaultConfig :: Config
defaultConfig = Config
  { cRootPrefix = "@"
  , cPlugins    = [ Fund.boot ]
  }
