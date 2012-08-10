------------------------------------------------------------------------------
-- File:          Plugins/MPlay.hs
-- Creation Date: Aug 05 2012 [05:37:06]
-- Last Modified: Aug 06 2012 [05:13:29]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Plugins.MPlay where

import Plugins

boot :: IO Plugin
boot = do
    env <- share Env
    return $ Plugin { pluginPersist = env
                    , pluginUni     = handleUni
                    , pluginRoot    = pluginRoot_ }

data Env = Env

handleUni :: Shared Env -> IrcMessage -> Con Result
handleUni env msg = return $ ResNone
