------------------------------------------------------------------------------
-- File:          Haikubot/CLI.hs
-- Creation Date: Dec 30 2012 [03:42:25]
-- Last Modified: Dec 31 2012 [08:53:45]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Haikubot.CLI where

import           Control.Monad
import qualified Data.Text.IO   as T
import           System.IO      (hFlush, stdout)
import           Haikubot.Core
import           Haikubot.Commands

-- | CLI interface main loop.
runCLI :: Handler ()
runCLI = forever $ do
    line <- liftIO
        $ T.putStr "haikubot> "
        >> hFlush stdout
        >> liftIO T.getLine
    _ <- runCmd' line
    return ()
