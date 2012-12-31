------------------------------------------------------------------------------
-- File: main.hs
-- Creation Date: Aug 04 2012 [21:37:25]
-- Last Modified: Dec 31 2012 [04:53:39]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

import Bot
import Config

main :: IO ()
main = mainWithCLI $ defaultConfig
