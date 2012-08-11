------------------------------------------------------------------------------
-- File: Config.hs
-- Creation Date: Aug 05 2012 [06:14:42]
-- Last Modified: Aug 09 2012 [19:42:23]
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
import qualified Plugins.Haiku as Haiku

basicConfig :: Config
basicConfig = defaultConfig
  { cPlugins = [ Fund.boot, MPlay.boot, Haiku.boot ] }

-- | default configuration, without plugins
defaultConfig :: Config
defaultConfig = Config
  { cRootPrefix = "@"
  , cPlugins    = [ Fund.boot ]
  }
