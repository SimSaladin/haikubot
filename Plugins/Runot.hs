------------------------------------------------------------------------------
-- File:          Plugins/Runot.hs
-- Creation Date: Dec 29 2012 [19:38:44]
-- Last Modified: Dec 29 2012 [19:56:34]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Plugins.Runot (Env(..)) where

import Plugin

data Env = Env { eChannels   :: [ByteString] -- ^ Channels where only haikus are permitted
               , eHaikuFile  :: FilePath     -- ^ File to save/read haikus
               , eHaikuFails :: Map.Map ByteString Int
               }
