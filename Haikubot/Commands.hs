------------------------------------------------------------------------------
-- File:          Haikubot/Commands.hs
-- Creation Date: Dec 30 2012 [04:09:45]
-- Last Modified: Dec 31 2012 [09:41:33]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Haikubot.Commands
  ( Cmd
  , runCmd
  , runCmd'
  , cmdsFromFile
  , runMsg
  ) where

import Data.Text (Text)
import Control.Monad (liftM)
import Data.Monoid (mconcat)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (openFile, hClose, IOMode(ReadMode))

import Haikubot.Core
import Haikubot.Actions
import Haikubot.Messages

type Cmd = (Text, [Text])

runMsg :: Maybe Con -> Maybe IrcMessage -> Handler ()
runMsg mcon mmsg = liftM mconcat $ onPlugins mmsg mcon handlePrivmsg

-- | TODO: support quoting.
parseCmd :: Text -> Cmd
parseCmd t = (T.takeWhile (' '/=) s, T.words $ T.strip $ T.dropWhile (/=' ') s)
  where s = T.stripStart t

-- | Execute the Cmd.
runCmd' :: Text -> Handler ()
runCmd' = runCmd Nothing Nothing

-- | Execute the Cmd coming from an irc message.
runCmd :: Maybe IrcMessage -> Maybe Con -> Text -> Handler ()
runCmd mmsg mcon cmdtext = liftM mconcat $ onPlugins mmsg mcon (handleCmd $ parseCmd cmdtext)

-- | Read and execute commands from a file line by line.
cmdsFromFile :: FilePath -> Handler ()
cmdsFromFile file = do
    h  <- liftIO $ openFile file ReadMode
    xs <- liftIO $ T.hGetContents h
    mapM_ runCmd' $ T.lines xs
    liftIO $ hClose h

