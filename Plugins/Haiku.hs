------------------------------------------------------------------------------
-- File:          Haiku.hs
-- Creation Date: Aug 09 2012 [18:48:17]
-- Last Modified: Aug 09 2012 [22:21:54]
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
module Plugins.Haiku where

import Control.Monad
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.Random (randomIO)

import Plugins
import Utils

import Tavutus (tavutaRuno, printTavut)

boot :: IO Plugin
boot = do
  env <- share $ Env [] "/home/sim/docs/haikuja.txt"
  return $ Plugin { pluginPersist = env
                  , pluginUni     = handleUni
                  , pluginRoot    = handleRoot
                  }

data Env = Env { eChannel   :: ByteString
               , eHaikuFile :: FilePath
               }

handleRoot :: Shared Env -> IrcMessage -> Handler Result
handleRoot env IrcMessage{ mCode = mcode, mOrigin = origin, mMsg = msg }
  | otherwise = undefined

handleUni :: Shared Env -> IrcMessage -> Con Result
handleUni env IrcMessage{ mCode = mcode, mOrigin = origin, mMsg = msg }
  | mcode == "PRIVMSG" = case origin of
    Just dest -> do
        env' <- getShare env
        let notice r = case r of Nothing -> return ()
                                 Just r' -> write $ MNotice (eChannel env') (mOrigin ++ ": " ++ r')
        if dest == eChannel env'
          then handleHaiku (eHaikuFile env') msg' >>= reply
          else if prefixed
            then liftM2 notice reply =<< handleHaiku (eHaikuFile env') (T.drop 2 msg')
            else none
        where reply r = case r of
                  Nothing -> success "haiku success"
                  Just r' -> write (MPrivmsg dest $ encodeUtf8 r') >> reply Nothing
              prefixed = T.take 2 msg' == "; "
    Nothing -> none
  | otherwise = none
  where msg' = decodeUtf8 msg

handleHaiku :: FilePath -> Text -> Con (Maybe Text)
handleHaiku file text = case tavutaRuno (T.unpack text) of
  Left err    -> return $ Just $ T.pack err
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
  
getRandom :: FilePath -> Con Text
getRandom fp = do
  exists <- io $ doesFileExist fp
  if exists
    then do
      lns <- liftM lines . io $ readFile fp
      if length lns < 1
         then return "i haz no haikuz!"
         else do
          rnd <- io randomIO
          return $ lns !! (rnd `mod` (length lns))
    else return "i haz no haikufilez!!"

isHaiku :: [Int] -> Bool
isHaiku = (==) [5,7,5]

rytmit :: [[[String]]] -> [Int]
rytmit = map (foldl (+) 0 . map length)
