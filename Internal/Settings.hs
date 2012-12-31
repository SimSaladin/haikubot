------------------------------------------------------------------------------
-- File:          Internal/Settings.hs
-- Creation Date: Dec 30 2012 [03:40:24]
-- Last Modified: Dec 31 2012 [02:39:46]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
-- | Configuration and settings.
module Internal.Settings where

import           System.Environment (getArgs)
import           Internal.Types
import           Internal.Commands
import           Logging

-- | Read and parse the .rc
-- XXX: should be "read command line arguments"
readRC :: Handler ()
readRC = liftIO getArgs >>= \as -> case as of
    (filename:_) -> do
        logInfo' $ "sourcing file `" ++ filename ++ "`..."
        cmdsFromFile filename
    _ -> return ()
