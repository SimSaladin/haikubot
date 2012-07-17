{-# LANGUAGE OverloadedStrings #-}
import Data.List
import qualified System.IO.UTF8 as U
import Network
import System.IO
import System.Random (randomIO)
import System.Directory (doesFileExist)
import Control.Monad.Reader hiding (forever)
import Control.Exception
import Text.Printf
import Prelude hiding (catch)
import Tavutus (tavutaRuno, printTavut)

server :: HostName
server = "irc.paivola.fi"
port :: Integer
port   = 6667
nick :: String
nick   = "haiku"
haikufile :: FilePath
haikufile = "/home/sim/docs/haikut.txt"

data Bot = Bot { socket :: Handle }
type Net = ReaderT Bot IO

main :: IO ()
main = bracket connect disconnect loop
   where
      disconnect = hClose . socket
      loop st    = catch (runReaderT run st) (\(SomeException _) -> return ())

connect :: IO Bot
connect = notify $ do
   h <- connectTo server (PortNumber (fromIntegral port))
--   hSetEncoding h latin1
   hSetBuffering h NoBuffering
   return (Bot h)
   where
      notify = bracket_
         (printf "Connecting to %s..." server >> hFlush stdout)
         (putStrLn "Done")

run :: Net ()
run = do
   write "NICK" nick
   write "USER" (nick ++ " 0 * :haiku bot")
   asks socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
   s <- init `fmap` io (U.hGetLine h)
   io (putStrLn s)
   if ping s then pong s else eval (drop 1 s)
  where
   forever a = a >> forever a
   ping x  = "PING :" `isPrefixOf` x
   pong x  = write "PONG" (':' : drop 6 x)

eval :: String -> Net ()
eval s = case what of
   "PRIVMSG" -> evalPriv from to msg
   "INVITE"  -> joinTo msg
   "001"     -> afterConnect
   _         -> return ()
  where
   (from,(_:rest)) = span (/= ' ') s
   (what,(_:rest')) = span (/= ' ') rest
   (dest,(_:msg)) = span (/= ':') rest'
   to = init dest
   --admin x = ":bps!~bps@ssdesk.paivola.fi " `isPrefixOf` x

evalPriv :: String -> String -> String -> Net ()
evalPriv user to msg = if "!" `isPrefixOf` msg
                           then evalCmd user to cmd args
                           else if "#haiku" `isPrefixOf` to
                              then haikuProcess to msg
                              else return ()
   where ((prefix:cmd),args') = span (/= ' ') msg
         args = case args' of
            [] -> []
            xs -> tail xs
   
evalCmd :: String -> String -> String -> String -> Net ()
evalCmd "bps!~bps@ssdesk.paivola.fi" _  "join" here = joinTo here
evalCmd "bps!~bps@ssdesk.paivola.fi" _  "register" _ = register
evalCmd "bps!~bps@ssdesk.paivola.fi" _  "identify" _ = identify
evalCmd _                            to "haiku" _   = haikuRandom to
evalCmd _                            to ""     arg  = haikuProcess to arg
evalCmd _                            to cmd    _    = privmsg to ("mitä? `" ++ cmd ++ "' not found")

afterConnect :: Net ()
afterConnect = do
   identify
   joinTo "#haiku-testing"

register :: Net ()
register = do
   privmsg "NickServ" "REGISTER IamTehHaikuB0t simsaladin@paivola.fi"

identify :: Net ()
identify = do
   privmsg "NickServ" "IDENTIFY IamTehHaikuB0t"

privmsg :: String -> String -> Net ()
privmsg c s = write "PRIVMSG" (c ++ " :" ++ s)

write :: String -> String -> Net ()
write s t = do
   h <- asks socket
   _ <- io $ hPrintf h "%s %s\r\n" s t
   io $ printf    "> %s %s\n" s t

joinTo :: String -> Net ()
joinTo x = write "JOIN" x

success :: [String]
success = ["\\(^_^)/", "d(-_^)", "(*^^*)", "p(*^_^*)q", "\\(^O^)/"]
failed :: [String]
failed = [ "(;_.)", "(-_-)" , "(._.)"  , "(- -)", "-(T_T)-"
         , "(ToT)", "(;´ρ`)", "o(>< )o", "(>o<)", "((+_+))"
         ]

haikuProcess :: String -> String -> Net ()
haikuProcess to s = case tavutaRuno s of
   Left err -> privmsg to err
   Right parsed -> let
      counts = map (\sanat -> foldl (+) 0 $ map length sanat) parsed
      isHaiku = counts == [5,7,5]
      tavutettu = printTavut parsed
      pptavut = foldl1 (\x y -> x ++ ('-':y)) (map show counts)
      in if isHaiku
         then saveHaiku -- >>
--            privmsg to ("hienoa! " ++ success !! (rnd `mod` (length success)))
         else privmsg to ("mitä jos tarkistaisit?  " ++ pptavut ++ ": " ++ tavutettu)
   where
      saveHaiku = do
         exists <- io $ doesFileExist haikufile
         io $ (if exists then appendFile else writeFile) haikufile (s ++ "\n")

haikuRandom :: String -> Net ()
haikuRandom to = do
   exists <- io $ doesFileExist haikufile
   if exists
      then do
         lns <- liftM lines . io $ readFile haikufile 
         if length lns < 1
            then privmsg to "hmm.. yhtään haikua ei löytynyt"
            else do
               rnd <- io randomIO
               privmsg to (lns !! (rnd `mod` (length lns)))
      else privmsg to "hmm.. yhtään haikua ei löytynyt"

io :: IO a -> Net a
io = liftIO

