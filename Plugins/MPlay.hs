{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
------------------------------------------------------------------------------
-- File:          Plugins/MPlay.hs
-- Creation Date: Aug 05 2012 [05:37:06]
-- Last Modified: Aug 05 2012 [22:16:46]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Plugins.MPlay where

import Plugins

-- | Every plugin can have a custom defined persistent environment.
data Environment = Environment { something :: Bool }

-- | Initialize the plugin.
instantiate :: IO Plugin
instantiate = do
    env <- share (Environment True)
    return $ Plugin env handler

-- | XXX: Should take ConnectionId instead
handler :: Shared Environment -> Connection -> IrcMessage -> Handler ()
handler env con msg = return ()
