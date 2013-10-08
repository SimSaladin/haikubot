------------------------------------------------------------------------------
-- File: Bot.hs
-- Creation Date: Jul 23 2012 [11:10:43]
-- Last Modified: Oct 08 2013 [21:38:00]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

-- | Useful re-exports for use in plugins.
module Haikubot
  ( module Export -- this is not visible on haddock, but other modules are(!)
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
import Data.Monoid    as Export (mappend, (<>))
import Control.Monad  as Export
import System.Exit

-- | Exit haikubot. Calls the exit handlers on plugins.
exit :: Handler ()
exit = do
    void $ onPlugins Nothing Nothing (onExit >> noop)
    liftIO exitSuccess
