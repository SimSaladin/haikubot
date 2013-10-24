------------------------------------------------------------------------------
-- File:          Plugins/Runot.hs
-- Creation Date: Dec 29 2012 [19:38:44]
-- Last Modified: Oct 24 2013 [23:59:10]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Haikubot.Plugins.Runot
    ( Runot(..)
    , getRandomHaiku
    , format
    ) where

import           Haikubot
import           Tavutus (tavutaRuno, printTavut)
import qualified Tavutus

import           Data.Time
import qualified Data.List as L
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

type UserId     = Text
data Haiku      = Haiku UserId UTCTime Text deriving (Show, Read)
type Monogatari = (Text, Text, [Haiku]) -- ^ Origin, Title , haikus
type Runo       = Either Haiku Monogatari
data Runot      = Runot
  { rChannels   :: [Text]       -- ^ Channels where only haikus are permitted
  , rHaikuFile  :: FilePath     -- ^ File to save/read haikus
  , rHaikuFails :: Map.Map Text Int
  , rHaikuMonogataries :: TMVar (Map Text Monogatari) -- Channel/user -> (title, haikut)
  }

instance HaikuPlugin Runot where
    handleCmd ("haiku?", [x]) = do
        mget mNick >>= guard . (== Just "bps") -- reply . T.pack . show -- >>= guard . (== Just "bps")
        aget rHaikuFile
            >>= liftIO . (getHaiku (read $ T.unpack x) >=> format)
            >>= mapM_ reply >> stop

    handleCmd ("haiku?", _)           = aget rHaikuFile
                                        >>= liftIO . (getRandomHaiku >=> format)
                                        >>= mapM_ reply >> stop
    handleCmd ("haiku", xs)           = doHaiku (T.unwords xs)
    handleCmd ("monogatari", ["end"]) = endMonogatari                  >> stop
    handleCmd ("monogatari", ["my", "impl", title]) = implicitMonogatari title
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
    Left  err  -> reply $ "Osaatko kirjoittaa? "
                       <> (T.pack . unwords . lines $ show err)
    Right parsed -> do
        whoami  <- liftM (fromMaybe "(no-one)") maybeOrigin
        time    <- liftIO getCurrentTime
        let haiku = Haiku whoami time msg
            in handleHaiku haiku parsed
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
    
setHaikus :: [Either Haiku Monogatari] -> Action Runot ()
setHaikus haikus = do
        haikuFile <- aget rHaikuFile
        liftIO . writeFile haikuFile . unlines $ map show haikus

handleHaiku :: Haiku -> Tavutus.Runo -> Action Runot ()
handleHaiku haiku tavut 
    | isHaiku (rytmit tavut) = do
        whoami  <- liftM (fromMaybe "(no-one)") maybeOrigin
        ref     <- aget rHaikuMonogataries
        users   <- liftIO . atomically $ takeTMVar ref
        users'  <- case Map.lookup whoami users of
            Nothing         -> saveHaiku (Left haiku) >> return users
            Just (a, b, xs) -> return $ Map.insert whoami (a, b, xs ++ [haiku]) users
        liftIO . atomically $ putTMVar ref users'
    | otherwise = void . reply . T.pack
                    $ "onko näin? "
                    <> L.intercalate "-" (map show $ rytmit tavut)
                    <> ": " <> printTavut tavut

getHaiku :: Int -> FilePath -> IO (Either Text Runo)
getHaiku nth fp = do
    liftM (Right . read . (!! nth) . lines) (readFile fp)

getRandomHaiku :: FilePath -> IO (Either Text Runo)
getRandomHaiku fp = do
  exists <- doesFileExist fp
  guard exists
  if exists
    then do
      lns <- liftM lines (readFile fp)
      if length lns >= 1
         then liftM (Right . read . (lns !!) . (`mod` length lns)) randomIO
         else return $ Left "i haz no haikuz!"
    else return $ Left "i haz no haikufilez!!"

format :: Either Text Runo -> IO [Text]
format (Left err)           = return [ "Error: " <> err <> ". This shouldn't happen." ]
format (Right (Left haiku)) = formatHaiku haiku >>= \x -> return [x]
format (Right (Right (origin, title, haikut))) = do
    liftM (("Monogatari: " <> title <> " (" <> origin <> ")") :)
        $ mapM (liftM ("  " <>) . formatHaiku) haikut

formatHaiku :: Haiku -> IO Text
formatHaiku (Haiku by time haiku) = liftM (\t -> haiku <> "  -- " <> by <> ", " <> t) $
    formatTime' time

formatTime' :: UTCTime -> IO Text
formatTime' time = liftM (T.pack .
    formatTime defaultTimeLocale "%k:%M (%d.%m-%y)" .
    flip utcToZonedTime time
    ) getCurrentTimeZone

isHaiku :: [Int] -> Bool
isHaiku = (==) [5,7,5]

rytmit :: Tavutus.Runo -> [Int]
rytmit = map $ sum . map length

-- * Monogataries

endMonogatari :: Action Runot ()
endMonogatari = do
  whoami <- requireOrigin
  ref    <- aget rHaikuMonogataries
  users  <- liftIO . atomically $ takeTMVar ref
  case Map.lookup whoami users of
      Nothing         -> void $ reply  "Ei monogataria sinulle"
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
      f _user monog = saveHaiku (Right monog)

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

-- | Get and save implicit monogatari, if available
implicitMonogatari :: Text -> Action Runot Res
implicitMonogatari title = do
    whoami <- requireOrigin
    haikuFile <- aget rHaikuFile
    exists <- liftIO $ doesFileExist haikuFile
    if exists
        then do
            haikus <- liftIO $ liftM (map read . reverse . lines) $ readFile haikuFile
            let ismine (Left (Haiku author _ _)) = whoami == author
                ismine _                         = False
                unLeft (Left x) = x
                unLeft       _  = error "unLeft: not Left"
            case takeWhile ismine $ reverse haikus of
                [] -> reply "Yhtään haikua ei löytynyt monogatariin"
                xs -> do
                    let monog = (whoami, title, map unLeft xs) -- mgs. are not recursive :)
                        rest  = reverse (Right monog : drop (length xs) haikus)
                    setHaikus rest
                    void $ reply $ "Implisiittinen monogotari: " <> title
                    distributeMonog monog
                    stop
        else reply "Error: no haiku file(!)"

distributeMonog :: Monogatari -> Action Runot ()
distributeMonog monog = do
    channels <- aget rChannels
    origin   <- mget mOrigin
    forM_ channels $ \ch -> do
        guard $ maybe False (/= ch) origin
        str <- liftIO . format . Right $ Right monog
        forM_ str $ privmsg ch
