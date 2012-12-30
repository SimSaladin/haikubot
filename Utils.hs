------------------------------------------------------------------------------
-- File: Utils.hs
-- Creation Date: Aug 06 2012 [00:20:11]
-- Last Modified: Dec 30 2012 [05:20:44]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
-- | Some commonly used functions to make life easier
module Utils where

import Prelude
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class

if' :: Bool -> a -> a -> a
if' test t e = if test then t else e

io :: MonadIO m => IO a -> m a
io = liftIO
