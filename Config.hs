------------------------------------------------------------------------------
-- File: Config.hs
-- Creation Date: Aug 05 2012 [06:14:42]
-- Last Modified: Aug 05 2012 [21:57:20]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Config 
  ( defaultConfig
  , basicConfig
  , Persist(..)
  ) where

import Data.Text (Text)
import Handler
import qualified Plugins.MPlay as MPlay

-- | default configuration
defaultConfig :: Config
defaultConfig = Config []

basicConfig :: Config
basicConfig = Config [MPlay.instantiate] --  [ (MPlay.init, return ()) ]

