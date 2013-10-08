------------------------------------------------------------------------------
-- File:          Plugins/Runot.hs
-- Creation Date: Dec 29 2012 [19:38:44]
-- Last Modified: Oct 06 2013 [13:37:36]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Haikubot.Plugins.Runot (Runot(..)) where

import           Haikubot
import           Tavutus (tavutaRuno, printTavut)

import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Concurrent.STM
import           Control.Applicative
import           Control.Arrow
import           Data.Monoid
import           Data.Maybe
import qualified Data.Text        as T
import           System.Directory (doesFileExist)
import           System.Random    (randomIO)

import           Control.Monad.Error     hiding (lift)

if' :: Bool -> a -> a -> a
if' ehto sitten muuten = if ehto then sitten else muuten

type Haiku = (Text, Either Text Monogatari)
type Monogatari = (Text, [Text])

data Runot = Runot
  { rChannels   :: [Text]       -- ^ Channels where only haikus are permitted
  , rHaikuFile  :: FilePath     -- ^ File to save/read haikus
  , rHaikuFails :: Map.Map Text Int
  , rHaikuMonogataries :: TMVar (Map Text Monogatari) -- Channel/user -> (title, haikut)
  }

instance HaikuPlugin Runot where
  handleCmd ("haiku?", _) = aget rHaikuFile >>= liftIO . getRandomHaiku >>= reply . format

  handleCmd ("haiku", xs) = let xs' = T.unwords xs in do
      case tavutaRuno (T.unpack xs') of
        Left err    -> reply $ "Osaatko kirjoittaa? " <> (T.pack . unwords . lines $ show err)
        Right tavut -> handleHaiku xs' tavut

  handleCmd ("monogatari", ["end"]) = endMonogatari
  handleCmd ("monogatari", xs)      = startMonogatari (T.unwords xs)
  handleCmd (_, _) = return ()

  handlePrivmsg = do
      throwError "undefined"

endMonogatari :: Action Runot ()
endMonogatari = do
  ref    <- aget rHaikuMonogataries
  whoami <- requireOrigin
  users  <- liftIO . atomically $ takeTMVar ref

  case Map.lookup whoami users of
      Nothing         -> reply $ "Ei monogataria sinulle"
      Just monogatari -> do
          liftIO . atomically $ putTMVar ref $ Map.delete whoami users
          saveHaiku (whoami, Right monogatari)

startMonogatari :: Text -> Action Runot ()
startMonogatari title = do
  ref <- aget rHaikuMonogataries
  whoami <- requireOrigin

  liftIO . atomically $ do
      users <- takeTMVar ref
      let f Nothing        = (title, [])
          f (Just (_, xs)) = (title, xs) -- rename
          in putTMVar ref $ Map.alter (Just . f) whoami users

saveHaiku :: Haiku -> Action Runot ()
saveHaiku x = do
    haikuFile <- aget rHaikuFile
    liftIO $ do
        exists <- doesFileExist haikuFile
        (if' exists appendFile writeFile) haikuFile (show x ++ "\n")

handleHaiku :: Text -> [[[String]]] -> Action Runot ()
handleHaiku haiku tavut = if isHaiku rytmi
    then do
        whoami  <- liftM (fromMaybe "(no-one)") maybeOrigin
        ref     <- aget rHaikuMonogataries
        users   <- liftIO . atomically $ takeTMVar ref
        case Map.lookup whoami users of
            Nothing -> saveHaiku (whoami, Left haiku)
            Just x  -> liftIO . atomically . putTMVar ref 
                        $ Map.insert whoami ((++ [haiku]) `second` x) users
    else reply . T.pack $ "onko näin? " ++ pptavut ++ ": " ++ printTavut tavut
  where
      rytmi   = rytmit tavut
      pptavut = foldl1 (\x y -> x ++ ('-':y)) (map show rytmi)

getRandomHaiku :: FilePath -> IO (Either Text Haiku)
getRandomHaiku fp = do
  exists <- doesFileExist fp
  guard exists
  if exists
    then do
      lns <- liftM lines (readFile fp)
      if length lns >= 1
         then liftM (Right . read . (!!) lns . (`mod` (length lns))) randomIO
         else return $ Left "i haz no haikuz!"
    else return $ Left "i haz no haikufilez!!"

format :: Either Text Haiku -> Text
format (Left err)          = "(An error occurred: " <> err <> ", contact administrator)"
format (Right (my, haiku)) = f haiku <> " -- " <> my
    where f (Left x)        = x
          f (Right (t, xs)) = t <> ": " <> T.intercalate "\n" (map ("  " <>) xs)

isHaiku :: [Int] -> Bool
isHaiku = (==) [5,7,5]

rytmit :: [[[String]]] -> [Int]
rytmit = map (sum . map length)
