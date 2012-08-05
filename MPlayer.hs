------------------------------------------------------------------------------
-- File: MPlayer.hs
-- Creation Date: Jul 23 2012 [11:10:22]
-- Last Modified: Aug 04 2012 [22:10:10]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
-- | Plugin for haikubot, which enables playing media via mplayer.
module MPlayer where

import Prelude
import Plugin

type MPlayerModule = []

instance Module MPlayerModule where

-- | Opens file in mplayer (and return something?)
play :: FilePath -> IO ()
play fp = do
    return ()
