------------------------------------------------------------------------------
-- File: Mainplus.hs
-- Creation Date: Jul 23 2012 [11:10:43]
-- Last Modified: Jul 23 2012 [11:10:57]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
import Prelude hiding (catch)
import Data.List (delete)
import Network
import Network.IRC
import System.Exit (exitWith, ExitCode(..))
import System.IO
import qualified System.IO.UTF8 as U
import Control.Exception
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Arrow ((***))
import Text.Printf


data Bot = Bot { botSocket :: Handle
               , botNick :: UserName
               , botAdmins :: [UserName]
               }

type Net = ReaderT Bot IO

type Haiku = ReaderT (TMVar Bot) IO

-- * IO Monad

-- | Main function. Starts the simple command-line interface.
main :: IO ()
main = do
   bot <- atomically $ newEmptyTMVar
   runReaderT cmdline bot

-- | Starts a Bot's listener
startBot :: Bot -> IO ()
startBot bot = bracket (return bot) disconnect loop
   where disconnect = hClose . botSocket
         loop st = catch (runReaderT run st) (\(SomeException _) -> return ())

-- | Connect to a server and return the handle in a Bot.
connect :: ServerName -> Integer -> UserName -> IO Bot
connect server port nick' = notify $ do
   h <- connectTo server (PortNumber $ fromIntegral port)
   hSetBuffering h NoBuffering
   return $ Bot h nick' []
   where
      notify = bracket_ (printf "Connecting to %s..." server >> hFlush stdout)
                        (putStrLn "Done")

-- * Haiku monad

-- | command line interface. Has access to the bots.
cmdline :: Haiku ()
cmdline = forever $ do
   cmd <- liftIO $ do
      putStr "haikubot> "
      hFlush stdout
      getLine
   execCmd cmd

-- | not sure if this could be done in a better way?
net :: (a -> Haiku ()) -> Net a -> Haiku ()
net f v = do
   mb <- tryReadBot
   case mb of
      Just bot -> do
         l <- liftIO $ runReaderT v bot
         f l
      Nothing -> glog "No bot connected"

none = const (return ())

-- | Parses a command line and executes corresponding action.
execCmd :: String -> Haiku ()
execCmd [] = return ()
execCmd s
   | is "quit" = saveAndQuit
   | is "join" = net none $ parseJoin args
   | is "part" = net none $ parsePart args
   | is "admin"   = net saveBot $ parseAdmin args
   | is "deadmin" = net saveBot $ parseDeadmin args
   | is "connect" = parseConnect args
   | otherwise    = err ("unknown command: " ++ s)
   where
      (cmd,args) = (id *** words) $ span (/= ' ') s -- TODO: support quoted arguments?
      is         = (==) cmd

-- | Parses a connect command and connects if no bot is currently connected.
parseConnect :: [String] -> Haiku ()
parseConnect args = case length args of
      3 -> tryReadBot >>= \mbot -> case mbot of
         Nothing -> do
            bot <- liftIO $ connect ip port (args !! 2)
            liftIO =<< asks (\x -> atomically $ putTMVar x bot)
            _ <- liftIO $ forkIO $ startBot bot
            return ()
         Just bot -> glog "Could not connect: already connected."

      _ -> glog ("Syntax: connect <address> <port> <nick>")
   where ip   = args !! 0
         port = read (args !! 1)

saveAndQuit :: Haiku ()
saveAndQuit = liftIO $ putStrLn "Exiting haikubot" >> exitWith ExitSuccess

tryReadBot :: Haiku (Maybe Bot)
tryReadBot = liftIO =<< asks (atomically . \x -> tryTakeTMVar x >>= \mvar -> case mvar of
      Just var -> putTMVar x var >> return mvar
      Nothing -> return mvar)

saveBot :: Bot -> Haiku ()
saveBot bot = asks (atomically . flip swapTMVar bot) >>= liftIO >> return ()

glog :: String -> Haiku ()
glog = liftIO . putStrLn . (++) "[haiku] "

err :: String -> Haiku ()
err = liftIO . putStrLn

writeH :: Message -> Haiku ()
writeH msg = tryReadBot >>= \mbot -> case mbot of
      Just bot -> liftIO $ socketWrite (botSocket bot) (botNick bot) msg
      Nothing  -> glog "Failed to write to socket: not connected"

-- * Net Monad

logb :: String -> Net ()
logb msg = asks botNick >>= \n -> liftIO $ putStrLn ("[" ++ n ++ "] " ++ msg)

errb :: String -> Net ()
errb = liftIO . putStrLn

parseJoin :: [String] -> Net ()
parseJoin args = if length args /= 1
   then logb "Syntax: join <channel>"
   else write $ joinChan (args !! 0)

parsePart :: [String] -> Net ()
parsePart args = if length args /= 1
   then logb "Syntax: part <channel>"
   else write $ part (args !! 0)

parseAdmin :: [String] -> Net Bot
parseAdmin args = do
   bot <- ask
   let admins = botAdmins bot
      in case length args of
         0 -> logb "Admins:" >> mapM_ (logb . (++) "  ") admins >> return bot
         1 -> return $ bot { botAdmins = (args !! 0) : admins }
         _ -> errb "Syntax: admin <nick>" >> return bot

parseDeadmin :: [String] -> Net Bot
parseDeadmin args = do
   bot <- ask
   case length args of
      1 -> return $ bot { botAdmins = delete (args !! 0) (botAdmins bot)}
      _ -> errb "Syntax: deadmin <nick>" >> return bot

write :: Message -> Net ()
write msg = do
   h <- asks botSocket
   n <- asks botNick
   io $ socketWrite h n msg

run :: Net ()
run = do
   asks botNick >>= \n -> write (nick n) >> write (user n "0" "*" "Bot")
   asks botSocket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
   n <- asks botNick
   s <- liftIO (U.hGetLine h) -- whii, we has unicode
   case decode s of
      Just msg -> debug n (encode msg) >> parseMsg msg
      Nothing  -> debug n "<<<Not irc message>>>"

parseMsg :: Message -> Net ()
parseMsg msg = asks botNick >>= \n -> case msg_command msg of
   "PING"    -> write $ msg { msg_command = "PONG" }
   "NOTICE"  -> debug n $ showMessage msg
   "MODE"    -> debug n ("mode changed: " ++ (show $ msg_params msg))
   "PRIVMSG" -> return ()
   _         -> return () -- debug n ("Unknown command: "++cmd)

debug :: UserName -> String -> Net ()
debug n m = io $ putStrLn ("[" ++ n ++ ":recv] " ++ m)

-- * Helpers

io :: IO a -> ReaderT Bot IO a
io = liftIO

if' :: Bool -> a -> a -> a
if' test t f = if test then t else f

socketWrite :: Handle -> UserName -> Message -> IO ()
socketWrite h n msg = do
   hPutStrLn h $ encode msg
   putStrLn ("[" ++ n ++ ":send] " ++ encode msg)
