------------------------------------------------------------------------------
-- File: Utils.hs
-- Creation Date: Aug 06 2012 [00:20:11]
-- Last Modified: Aug 06 2012 [04:41:08]
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

cmdSplit :: Text -> (Text, Text)
cmdSplit t = (T.takeWhile (' '/=) s, T.strip $ T.dropWhile (/=' ') s)
  where s = T.stripStart t
