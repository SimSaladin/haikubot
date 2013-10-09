------------------------------------------------------------------------------
-- File:          Plugins/Runot.hs
-- Creation Date: Dec 29 2012 [19:38:44]
-- Last Modified: Oct 09 2013 [20:07:47]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Haikubot.Plugins.Runot
    ( Runot(..)
    , getRandomHaiku
    , format
    ) where

import           Haikubot
import           Tavutus (tavutaRuno, printTavut)

import           Data.Time
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Concurrent.STM
import           Data.Maybe
import qualified Data.Text        as T
import           System.Directory (doesFileExist)
import           System.Random    (randomIO)
import           System.Locale

if' :: Bool -> a -> a -> a
if' ehto sitten muuten = if ehto then sitten else muuten

type UserId = Text

data Haiku = Haiku UserId UTCTime Text
             deriving (Show, Read)

type Monogatari = (Text, Text, [Haiku]) -- ^ Origin, Title , haikus

type Runo = Either Haiku Monogatari

data Runot = Runot
  { rChannels   :: [Text]       -- ^ Channels where only haikus are permitted
  , rHaikuFile  :: FilePath     -- ^ File to save/read haikus
  , rHaikuFails :: Map.Map Text Int
  , rHaikuMonogataries :: TMVar (Map Text Monogatari) -- Channel/user -> (title, haikut)
  }

instance HaikuPlugin Runot where
    handleCmd ("haiku?", _)           = aget rHaikuFile
                                        >>= liftIO . (getRandomHaiku >=> format)
                                        >>= mapM_ reply >> stop
    handleCmd ("haiku", xs)           = doHaiku (T.unwords xs)
    handleCmd ("monogatari", ["end"]) = endMonogatari                  >> stop
    handleCmd ("monogatari", xs)      = startMonogatari (T.unwords xs) >> stop
    handleCmd (_, _)                  = noop

    handleIrcMessage IrcMessage
        { mCode   = "PRIVMSG"
        , mOrigin = Just origin
        , mMsg    = msg
        } = do guard . elem origin =<< aget rChannels
               doHaiku msg
    handleIrcMessage _ = noop

    onExit = endMonogataries

doHaiku :: Text -> Action Runot Res
doHaiku msg = case tavutaRuno (T.unpack msg) of
    Left  err   -> reply $ "Osaatko kirjoittaa? "
                        <> (T.pack . unwords . lines $ show err)
    Right tavut -> do
        whoami  <- liftM (fromMaybe "(no-one)") maybeOrigin
        time    <- liftIO getCurrentTime
        let haiku = Haiku whoami time msg
            in handleHaiku haiku tavut
        stop

saveHaiku :: Runo -> Action Runot ()
saveHaiku (Right (_, title, [])) = void . reply $ "Monogatari "
            <> title
            <> " suljettiin, mutta siihen ei kuulu yhtään haikua, joten sitä huomioitu."
saveHaiku x = do
    haikuFile <- aget rHaikuFile
    liftIO $ do exists <- doesFileExist haikuFile
                (if' exists appendFile writeFile) haikuFile (show x ++ "\n")
    case x of
        (Right (_, title, xs)) -> void $ reply $ "Uusi monogatari: " <> title <>
                                                 " (" <> (T.pack $ show $ length xs) <> " haikua)."
        _ -> return ()
    

handleHaiku :: Haiku -> [[[String]]] -> Action Runot ()
handleHaiku haiku tavut = if isHaiku rytmi
    then do
        whoami  <- liftM (fromMaybe "(no-one)") maybeOrigin
        ref     <- aget rHaikuMonogataries
        users   <- liftIO . atomically $ takeTMVar ref
        case Map.lookup whoami users of
            Nothing         -> saveHaiku (Left haiku)
            Just (a,b,xs)   -> liftIO . atomically . putTMVar ref 
                        $ Map.insert whoami (a, b, xs ++ [haiku]) users
    else void . reply . T.pack $ "onko näin? " ++ pptavut ++ ": " ++ printTavut tavut
  where
      rytmi   = rytmit tavut
      pptavut = foldl1 (\x y -> x ++ ('-':y)) (map show rytmi)

getRandomHaiku :: FilePath -> IO (Either Text Runo)
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

format :: Either Text Runo -> IO [Text]
format (Left err)           = return [ "Error: " <> err <> ". This shouldn't happen." ]
format (Right (Left haiku)) = formatHaiku haiku >>= \x -> return [x]
format (Right (Right (origin, title, haikut))) = do
    liftM ( ("Monogatari: "<>title<>" ("<>origin<>")") :)
        $ mapM (liftM ("  " <>) . formatHaiku) haikut

formatHaiku :: Haiku -> IO Text
formatHaiku (Haiku by time haiku) = liftM (\t -> haiku <> "  -- " <> by <> ", " <> t) $ formatTime' time

formatTime' :: UTCTime -> IO Text
formatTime' time = liftM (T.pack . formatTime defaultTimeLocale "%k:%M (%d.%m-%y)" . flip utcToZonedTime time) getCurrentTimeZone

isHaiku :: [Int] -> Bool
isHaiku = (==) [5,7,5]

rytmit :: [[[String]]] -> [Int]
rytmit = map (sum . map length)


-- * Monogataries

endMonogatari :: Action Runot ()
endMonogatari = do
  ref    <- aget rHaikuMonogataries
  whoami <- requireOrigin
  users  <- liftIO . atomically $ takeTMVar ref

  case Map.lookup whoami users of
      Nothing         -> void . reply $ "Ei monogataria sinulle"
      Just monogatari -> do
          liftIO . atomically $ putTMVar ref $ Map.delete whoami users
          distributeMonog monogatari
          saveHaiku (Right monogatari)

-- | End and save all monogataries
endMonogataries :: Action Runot ()
endMonogataries = do
    ref <- aget rHaikuMonogataries
    (liftIO . atomically . takeTMVar) ref >>= sequence_ . Map.elems . Map.mapWithKey f
    (liftIO . atomically . putTMVar ref) Map.empty
  where
      f user monog = saveHaiku (Right monog)

startMonogatari :: Text -> Action Runot ()
startMonogatari title = do
    ref    <- aget rHaikuMonogataries
    whoami <- requireOrigin

    liftIO . atomically $ do
      users <- takeTMVar ref
      let f Nothing                = (whoami, title, [])
          f (Just (origin, _, xs)) = (origin, title, xs) -- rename monogatari
          in putTMVar ref $ Map.alter (Just . f) whoami users

    liftIO $ atomically (readTMVar ref) >>= print . show

distributeMonog :: Monogatari -> Action Runot ()
distributeMonog monog = do
    channels <- aget rChannels
    origin   <- mget mOrigin
    forM_ channels $ \ch -> do
        guard $ maybe False (/= ch) origin
        str <- liftIO . format . Right $ Right monog
        forM_ str $ privmsg ch
