------------------------------------------------------------------------------
-- File:          Haikubot/Settings.hs
-- Creation Date: Dec 30 2012 [03:40:24]
-- Last Modified: Dec 31 2012 [09:03:43]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
-- | Configuration and settings.
module Haikubot.Settings where

import           System.Environment (getArgs)
import           Haikubot.Core
import           Haikubot.Commands
import           Haikubot.Logging

-- | Read and parse the .rc
-- XXX: should be "read command line arguments"
readRC :: Handler ()
readRC = liftIO getArgs >>= \as -> case as of
    (filename:_) -> do
        logInfo' $ "sourcing file `" ++ filename ++ "`..."
        cmdsFromFile filename
    _ -> return ()
