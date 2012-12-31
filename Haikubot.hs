------------------------------------------------------------------------------
-- File: Bot.hs
-- Creation Date: Jul 23 2012 [11:10:43]
-- Last Modified: Dec 31 2012 [09:48:03]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

-- | Useful re-exports for use in plugins.
module Haikubot
  ( module Export
  , module Haikubot.Actions
  , module Haikubot.Connections
  , module Haikubot.Core
  , module Haikubot.Logging
  , module Haikubot.Messages
  , exit
  ) where

import Haikubot.Actions
import Haikubot.Connections
import Haikubot.Core
import Haikubot.Logging
import Haikubot.Messages
import Data.Text      as Export (Text)
import Data.Monoid    as Export (mappend)
import Control.Monad  as Export
import System.Exit

-- | Exit haikubot. Call the exit handler on plugins first.
exit :: Handler ()
exit = do
    _ <- onPlugins Nothing Nothing onExit
    liftIO exitSuccess
