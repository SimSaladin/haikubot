------------------------------------------------------------------------------
-- File:          Plugins/Runot.hs
-- Creation Date: Dec 29 2012 [19:38:44]
-- Last Modified: Apr 11 2013 [22:32:00]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Haikubot.Plugins.Runot (Runot(..)) where

import           Haikubot
import           Tavutus (tavutaRuno, printTavut)

import qualified Data.Text        as T
import qualified Data.Map         as Map
import           System.Directory (doesFileExist)
import           System.Random    (randomIO)

data Runot = Runot
  { rChannels   :: [Text]       -- ^ Channels where only haikus are permitted
  , rHaikuFile  :: FilePath     -- ^ File to save/read haikus
  , rHaikuFails :: Map.Map Text Int
  }

instance HaikuPlugin Runot where
  handleCmd ("haiku?", _) = aget rHaikuFile >>= liftIO . getRandomHaiku >>= reply . format

  handleCmd ("haiku", xs) = do
      case tavutaRuno (T.unpack xs') of
        Left err    -> reply $ "Osaatko kirjoittaa? " `T.append` (T.pack err)
        Right tavut -> handleHaiku xs' tavut
    where xs' = T.intercalate " " xs

  handleCmd (_,_) = return ()

handleHaiku :: Text -> [[[String]]] -> Action Runot ()
handleHaiku haiku tavut = if isHaiku rytmi
    then liftIO . flip saveHaiku haiku =<< aget rHaikuFile
    else reply $ T.pack $ "onko näin? " ++ pptavut ++ ": " ++ printTavut tavut
    where rytmi   = rytmit tavut
          pptavut = foldl1 (\x y -> x ++ ('-':y)) (map show rytmi)

saveHaiku :: FilePath -> Text -> IO ()
saveHaiku file haiku = do
  exists <- doesFileExist file
  (if exists then appendFile else writeFile) file (T.unpack haiku ++ "\n")

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

format :: Either Text Text -> Text
format (Left err)    = "An error occurred: " `T.append` err
format (Right haiku) = haiku

isHaiku :: [Int] -> Bool
isHaiku = (==) [5,7,5]

rytmit :: [[[String]]] -> [Int]
rytmit = map (foldl (+) 0 . map length)
