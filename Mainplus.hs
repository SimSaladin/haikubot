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

-- | Starts a bot in a new thread (?)
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

-- | Parses a command line and executes corresponding action.
execCmd :: String -> Haiku ()
execCmd [] = return ()
execCmd s
   | is "quit" = saveAndQuit
   | is "join" = if length args /= 1
                     then err "Syntax: join <channel>"
                     else writeH $ joinChan (args !! 0)
   | is "part" = if length args /= 1
                     then err "Syntax: part <channel>"
                     else writeH $ part (args !! 0)

   | is "admin"   = tryReadBot >>= \mbot -> case (mbot, length args) of
      (Nothing,  _) -> err "Could not admin: not connected"
      (Just bot, 0) -> glog "Admins:" >> mapM_ (glog . (++) "  ") (botAdmins bot) -- TODO: print admins
      (Just bot, 1) -> saveBot $ bot { botAdmins = (args !! 0) : botAdmins bot }
      (_,        _) -> err "Syntax: admin <nick>"

   | is "deadmin" = tryReadBot >>= \mbot -> case (mbot, length args) of
      (Nothing,  _) -> err "Could not deadmin: no bot initialized"
      (Just bot, 1) -> saveBot $ bot { botAdmins = delete (args !! 0) (botAdmins bot)}
      (_,        _) -> err "Syntax: deadmin <nick>"

   | is "connect" = parseConnect args
   | otherwise    = liftIO $ putStrLn ("unknown command: " ++ s)
   where
      (cmd,args) = (id *** words) $ span (/= ' ') s -- TODO: support quoted arguments?
      is         = (==) cmd

-- | Parses a connect command
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
