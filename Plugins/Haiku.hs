------------------------------------------------------------------------------
-- File:          Haiku.hs
-- Creation Date: Aug 09 2012 [18:48:17]
-- Last Modified: Dec 31 2012 [04:45:56]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
-- | 
--  Message parsing:
--    `[; ]<haiku>` -- check if <haiku> is really a haiku and log it if it is.
--                     The prefix is optional on channels/nicks listed in
--                     eHaikuOnly.
--
--  Commands:
--    haiku! -- reply a random haiku
--
module Plugins.Haiku (boot) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Map as Map
import System.Directory (doesFileExist)
import System.Random (randomIO)
import Plugins
import Utils
import Tavutus (tavutaRuno, printTavut)

boot :: IO (Text, Plugin)
boot = do
  env <- share $ Env ["#haiku", "#haiku-testing"] "/home/sim/docs/haikut.txt" Map.empty
  return $ (,) "Haiku" $ Plugin { pluginPersist = env
                                , pluginUni     = handleUni
                                , pluginRoot    = handleRoot
                                }

data Env = Env { eChannels   :: [ByteString] -- ^ Channels where only haikus are permitted
               , eHaikuFile  :: FilePath     -- ^ File to save/read haikus
               , eHaikuFails :: Map.Map ByteString Int
               }

handleRoot :: Shared Env -> Text -> Text -> Maybe IrcMessage -> Handler Result
handleRoot env cmd arg mmsg
  | cmd == "haiku?" = do
      a <- io . getRandomHaiku =<< liftM eHaikuFile (getShare env)
      case mmsg of
        Nothing -> failed "no origin"
        Just IrcMessage{mOrigin = morigin} -> case morigin of
          Nothing     -> failed "no origin"
          Just origin -> reply $ MPrivmsg origin $ encodeUtf8 $ case a of
            Left reason -> reason
            Right haiku -> haiku
  | cmd == "haiku" = do
      haikuFile <- liftM eHaikuFile $ getShare env
      mdesc <- handleHaiku haikuFile arg
      case mdesc of
        Nothing   -> success "haiku success"
        Just desc -> case mmsg of
          Nothing -> failed "origin n/a. haiku channels have been notified"
          Just IrcMessage{ mOrigin = morigin } -> case morigin of
            Just origin -> reply (MPrivmsg origin $ encodeUtf8 desc)
            Nothing     -> failed "no origin."
  | otherwise = none

handleUni :: Shared Env -> IrcMessage -> Handler Result
handleUni env IrcMessage{ mNick = mnick, mCode = mcode, mOrigin = morigin, mMsg = msg }
  | mcode == "PRIVMSG" = case (mnick, morigin) of
    (Just nick, Just origin) -> do
        env'@Env{ eChannels = channels, eHaikuFile = haikuFile, eHaikuFails = haikuFails } <- getShare env
        if origin `elem` channels
          then do
              mdesc <- handleHaiku haikuFile (if' prefixed (T.drop 2 msg') msg')
              case mdesc of
                Nothing   -> success "haiku success"
                Just desc -> reply (MPrivmsg origin $ encodeUtf8 desc)
          else none
    _ -> none
  | otherwise = none
  where
    msg'     = decodeUtf8 msg
    prefixed = T.take 2 msg' == "; "

--removeFails :: Env -> Text -> Handler ()
--removeFails env who = setShare $ env { eHaikuFails = Map.insert who 0 (eHaikuFails env) }
--
--handleFail :: Env -> Text -> Handler ()
--handleFail env who = let (mfails, hfails) = Map.insertLookupWithKey (\_ a b -> a + b) who 1 env
--                     in case mfails of
--                          Just fails -> if fails > 2 then kickUser who else return ()
--                          Nothing -> return ()
--                        setShare $ env { eHaikuFails = hfails }
--
--kickUser :: Text -> Handler ()
--kickUser who = reply $ MKick _ who ""

handleHaiku :: FilePath -- ^ haiku file
            -> Text     -- ^ text to parse
            -> Handler (Maybe Text) -- ^ `Nothing` if @text@ is haiku; `Just description` if not
handleHaiku file text = case tavutaRuno (T.unpack text) of
  Left err    -> return $ Just "osaatko edes kirjoittaa?" -- $ T.pack err
  Right tavut -> let
    rytmi   = rytmit tavut
    pptavut = foldl1 (\x y -> x ++ ('-':y)) (map show rytmi)
    in if isHaiku rytmi
      then save >> return Nothing
      else return $ Just $ T.pack $ "onko näin? " ++ pptavut ++ ": " ++ printTavut tavut
  where
    save = do
      exists <- io $ doesFileExist file
      io $ (if exists then appendFile else writeFile) file (T.unpack text ++ "\n")

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

isHaiku :: [Int] -> Bool
isHaiku = (==) [5,7,5]

rytmit :: [[[String]]] -> [Int]
rytmit = map (foldl (+) 0 . map length)
