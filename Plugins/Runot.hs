------------------------------------------------------------------------------
-- File:          Plugins/Runot.hs
-- Creation Date: Dec 29 2012 [19:38:44]
-- Last Modified: Dec 31 2012 [04:45:59]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Plugins.Runot where

import Bot
import qualified Data.Text as T
import qualified Data.Map as Map
import System.Directory (doesFileExist)
import System.Random (randomIO)

data Runot = Runot
  { rChannels   :: [Text]       -- ^ Channels where only haikus are permitted
  , rHaikuFile  :: FilePath     -- ^ File to save/read haikus
  , rHaikuFails :: Map.Map Text Int
  }

instance HaikuPlugin Runot where
  handleCmd ("haiku?", _) = do
      origin <- requireOrigin
      haiku  <- liftIO . getRandomHaiku =<< aget rHaikuFile 
      privmsg origin $ format haiku

format :: Either Text Text -> Text
format (Left err)    = "An error occurred: " `T.append` err
format (Right haiku) = haiku

getRandomHaiku :: FilePath -> IO (Either Text Text)
getRandomHaiku fp = do
  exists <- doesFileExist fp
  if exists
    then do
      lns <- liftM lines (readFile fp)
      if length lns >= 1
         then randomIO >>= return . Right . T.pack . (!!) lns . (`mod` (length lns))
         else return $ Left "i haz no haikuz!"
    else return $ Left "i haz no haikufilez!!"
