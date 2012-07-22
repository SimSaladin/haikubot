import Prelude hiding (catch)
import Network
import Network.IRC
import System.Exit (exitWith, ExitCode(..))
import System.IO
import qualified System.IO.UTF8 as U
import Control.Exception
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Arrow ((***))
import Text.Printf


data Bot = Bot { botSocket :: Handle
               , botNick :: UserName
               , botAdmins :: [UserName]
               }
type Net = ReaderT Bot IO
type Haiku = ReaderT (TMVar Bot) IO

if' :: Bool -> a -> a -> a
if' test t f = if test then f else t

-- | Main function. Starts the simple command-line interface.
main :: IO ()
main = do
   bot <- atomically $ newEmptyTMVar
   runReaderT cmdline bot

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
   | is "quit"    = saveAndQuit
   | is "join"    = return ()
   | is "connect" = parseConnect args
   | otherwise    = liftIO $ putStrLn ("unknown command: " ++ s)
   where
      (cmd,args') = (id *** join (liftM2 if' (not . null) init)) $ span (/= ' ') s
      is          = (==) cmd
      args        = words args' -- TODO: support quoted arguments?

parseConnect :: [String] -> Haiku ()
parseConnect args = case length args of
      3 -> do
         mbot <- tryReadBot
         liftIO $ putStrLn "read bot"
         case mbot of
            Nothing -> do
               bot <- liftIO $ connect ip port (args !! 2)
               liftIO =<< asks (\x -> atomically $ putTMVar x bot)
               liftIO $ startBot bot

            Just bot -> do
               liftIO $ putStrLn "Could not connect: already connected"

      _ -> liftIO $ putStrLn ("Syntax: connect <address> <port> <nick>")
   where ip   = args !! 0
         port = read (args !! 1)

tryReadBot :: Haiku (Maybe Bot)
tryReadBot = liftIO =<< asks (atomically . \x -> tryTakeTMVar x >>= \mvar -> case mvar of
      Just var -> putTMVar x var >> return mvar
      Nothing -> return mvar)

      -- isempty then return Nothing else return Nothing))
--   value <- asks (\x -> atomically $ isEmptyTMVar x >>= \isempty ->
--   if isempty then readTMVar x >>= return . Just else return Nothing)

saveAndQuit :: Haiku ()
saveAndQuit = liftIO $ putStrLn "Exiting haikubot" >> exitWith ExitSuccess

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

write :: Message -> Net ()
write msg = do
   h <- asks botSocket
   n <- asks botNick
   io $ hPutStrLn h $ encode msg
   io $ putStrLn ("[" ++ n ++ ":send] " ++ encode msg)

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

io :: IO a -> ReaderT Bot IO a
io = liftIO
