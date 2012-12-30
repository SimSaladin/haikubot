------------------------------------------------------------------------------
-- File:          Internal/CLI.hs
-- Creation Date: Dec 30 2012 [03:42:25]
-- Last Modified: Dec 30 2012 [04:03:02]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Internal.CLI where

import           Control.Monad
import qualified Data.Text.IO   as T
import           System.IO      (hFlush, stdout)
import           Internal.Types

-- | CLI interface main loop.
runCLI :: Handler ()
runCLI = forever $ liftIO $ do
    T.putStr "haikubot> "
    hFlush stdout
    T.getLine >>= rootCommand Nothing >>= logRes

