------------------------------------------------------------------------------
-- File:          Internal/Listen.hs
-- Creation Date: Dec 29 2012 [23:03:37]
-- Last Modified: Dec 29 2012 [23:50:07]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Internal.Listen where

import Control.Monad

import Internal.Types
import Utils

listener :: Con -> Handler ()
listener con = do
  forever $ io $ putStrLn "Not yet implemented"
