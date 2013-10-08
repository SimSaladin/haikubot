------------------------------------------------------------------------------
-- File:          Haikubot/Commands.hs
-- Creation Date: Dec 30 2012 [04:09:45]
-- Last Modified: Oct 08 2013 [20:44:43]
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
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (openFile, hClose, IOMode(ReadMode))

import Haikubot.Core
import Haikubot.Actions
import Haikubot.Messages

-- * Messages

-- | Run handlers for an privmsg
runMsg :: Con -> IrcMessage -> Handler PluginResult
runMsg con msg =
    onPlugins (Just con) (Just msg) (handleIrcMessage msg)

-- * Commands

type Cmd = (Text, [Text])

-- | Execute the Cmd.
runCmd' :: Text -> Handler ()
runCmd' = runCmd Nothing Nothing

-- | Execute the Cmd.
runCmd :: Maybe Con -> Maybe IrcMessage
       -> Text
       -> Handler ()
runCmd mcon mmsg cmdtext = void $
    onPlugins mcon mmsg (handleCmd $ parseCmd cmdtext)

-- | TODO: support quoting.
parseCmd :: Text -> Cmd
parseCmd t = (T.takeWhile (' '/=) s, T.words $ T.strip $ T.dropWhile (/=' ') s)
  where s = T.stripStart t

-- | Read and execute commands from a file line by line.
cmdsFromFile :: FilePath -> Handler ()
cmdsFromFile file = do
    h  <- liftIO $ openFile file ReadMode
    xs <- liftIO $ T.hGetContents h
    mapM_ runCmd' $ T.lines xs
    liftIO $ hClose h

