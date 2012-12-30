------------------------------------------------------------------------------
-- File:          Internal/Commands.hs
-- Creation Date: Dec 30 2012 [04:09:45]
-- Last Modified: Dec 30 2012 [06:27:39]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Internal.Commands
  ( Cmd
  , runCmd
  , runCmd'
  , cmdsFromFile
  ) where

import Data.Text (Text)
import Control.Monad (liftM)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (openFile, hClose, IOMode(ReadMode))

import Internal.Types
import Internal.Actions
import Internal.Messages
import Utils

type Cmd = (Text, [Text])

-- | TODO: support quoting.
parseCmd :: Text -> Cmd
parseCmd t = (T.takeWhile (' '/=) s, T.words $ T.strip $ T.dropWhile (/=' ') s)
  where s = T.stripStart t

runCmd :: Cmd -> Handler [Res]
runCmd = runCmd' Nothing

runCmd' :: Maybe IrcMessage -> Cmd -> Handler [Res]
runCmd' mmsg cmd = onPlugins mmsg (handleCommand cmd)

cmdsFromFile :: FilePath -> Handler ()
cmdsFromFile file = do
    h  <- liftIO $ openFile file ReadMode
    xs <- liftIO $ T.hGetContents h
    mapM_ (runCmd . parseCmd) $ T.lines xs
    liftIO $ hClose h
