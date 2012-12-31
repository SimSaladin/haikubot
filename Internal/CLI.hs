------------------------------------------------------------------------------
-- File:          Internal/CLI.hs
-- Creation Date: Dec 30 2012 [03:42:25]
-- Last Modified: Dec 31 2012 [02:35:31]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Internal.CLI where

import           Control.Monad
import qualified Data.Text.IO   as T
import           System.IO      (hFlush, stdout)
import           Internal.Types
import           Internal.Commands

-- | CLI interface main loop.
runCLI :: Handler ()
runCLI = forever $ do
    line <- liftIO
        $ T.putStr "haikubot> "
        >> hFlush stdout
        >> liftIO T.getLine
    _ <- runCmd' line
    return ()
