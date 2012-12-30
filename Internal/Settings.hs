------------------------------------------------------------------------------
-- File:          Internal/Settings.hs
-- Creation Date: Dec 30 2012 [03:40:24]
-- Last Modified: Dec 30 2012 [05:05:43]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
-- | Configuration and settings.
module Internal.Settings where

readRC :: Handler ()
readRC = getArgs >>= \as -> case as of
    (filename:_) -> do
        -- XXX: log instead?
        liftIO . putStrLn $ "sourcing file `" ++ filename ++ "`..."
        sourceCommands filename
      _ -> return ()
