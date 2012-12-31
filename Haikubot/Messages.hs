------------------------------------------------------------------------------
-- File:          Messages.hs
-- Creation Date: Dec 30 2012 [02:49:56]
-- Last Modified: Dec 31 2012 [03:04:12]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
-- 
-- |
-- Module : Network.SimpleIRC.Core
-- Copyright : (c) Dominik Picheta 2010
-- License : BSD3
--
-- Maintainer : morfeusz8@gmail.com
-- Stability : provisional
-- Portability : portable
--
-- Messages (parsing) module
--
------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Haikubot.Messages
  ( IrcMessage(..)
  , Command(..)
  , parse
  , showCommand
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow hiding (first)
import Data.Typeable

data Command =
    MPrivmsg Text Text              -- ^ PRIVMSG #chan :msg
  | MJoin    Text (Maybe Text)      -- ^ JOIN #chan key
  | MPart    Text Text              -- ^ PART #chan :msg
  | MMode    Text Text (Maybe Text) -- ^ MODE #chan +o user
  | MTopic   Text (Maybe Text)      -- ^ TOPIC #chan :topic
  | MInvite  Text Text              -- ^ INVITE user #chan
  | MKick    Text Text Text         -- ^ KICK #chan user :msg
  | MQuit    Text                   -- ^ QUIT :msg
  | MNick    Text                   -- ^ NICK newnick
  | MNotice  Text Text              -- ^ NOTICE usr/#chan :msg
  | MAction  Text Text              -- ^ PRIVMSG usr/#chan :ACTION msg
  | MUser    Text Text Text         -- ^ USER user mode :real
  deriving (Eq, Read, Show)

data IrcMessage = IrcMessage
  { mNick   :: Maybe Text
  , mUser   :: Maybe Text
  , mHost   :: Maybe Text
  , mServer :: Maybe Text
  , mCode   :: Text
  , mMsg    :: Text
  , mChan   :: Maybe Text
  , mOrigin :: Maybe Text   -- ^ Origin of the message, this is mNick if a message was sent directly to the bot, otherwise if it got sent to the channel it's mChan.
  , mOther  :: Maybe [Text]
  , mRaw    :: Text
  } deriving (Show, Typeable)

-- |Parse a raw IRC message
parse :: Text -> IrcMessage
parse txt =
  case split of
    [code, msg]                     -> parse2 code msg noCarriage
    [first, code, msg]              -> parse3 first code msg noCarriage
    [first, code, chan, msg]        -> parse4 first code chan msg noCarriage
    [first, code, chan, other, msg] -> parse5 first code chan other msg noCarriage
    server:code:nick:chan:other     -> parseOther server code nick chan other noCarriage
    _                               -> error "SimpleIRC: unexpected message format"

  where noCarriage = takeCarriageRet txt
        split      = smartSplit noCarriage

-- Nick, Host, Server
parseFirst :: Text -> (Maybe Text, Maybe Text, Maybe Text, Maybe Text)
parseFirst first =
  if (== '!') `T.any` first
    then let (nick, user_host) = T.break (== '!') (dropColon first)
         in if (== '@') `T.any` user_host
               then let (user, host) = second T.tail $ T.break (== '@') $ T.tail user_host
                    in (Just nick, Just user, Just host, Nothing)
               else (Just nick, Nothing, Just user_host, Nothing)
    else (Nothing, Nothing, Nothing, Just $ dropColon first)

getOrigin :: Maybe Text -> Text -> Text
getOrigin (Just nick) chan =
  if "#" `T.isPrefixOf` chan || "&" `T.isPrefixOf` chan || "+" `T.isPrefixOf` chan
      || "!" `T.isPrefixOf` chan
    then chan
    else nick
getOrigin Nothing chan = chan

parse2 :: Text -> Text -> Text -> IrcMessage
parse2 code msg =
  IrcMessage Nothing Nothing Nothing Nothing code
    (dropColon msg) Nothing Nothing Nothing

parse3 :: Text -> Text -> Text -> Text -> IrcMessage
parse3 first code msg =
  let (nick, user, host, server) = parseFirst first
  in IrcMessage nick user host server code (dropColon msg) Nothing Nothing Nothing

parse4 :: Text
       -> Text
       -> Text
       -> Text
       -> Text
       -> IrcMessage
parse4 first code chan msg =
  let (nick, user, host, server) = parseFirst first
  in IrcMessage nick user host server code
       (dropColon msg) (Just chan) (Just $ getOrigin nick chan) Nothing

parse5 :: Text
       -> Text
       -> Text
       -> Text
       -> Text
       -> Text
       -> IrcMessage
parse5 first code chan other msg =
  let (nick, user, host, server) = parseFirst first
  in IrcMessage nick user host server code
    (dropColon msg) (Just chan) (Just $ getOrigin nick chan) (Just [other])

parseOther :: Text
           -> Text
           -> Text
           -> Text
           -> [Text]
           -> Text
           -> IrcMessage
parseOther server code nick chan other =
  IrcMessage (Just nick) Nothing Nothing (Just server) code
    (T.unwords other) (Just chan) (Just $ getOrigin (Just nick) chan) (Just other)

smartSplit :: Text -> [Text]
smartSplit txt =
  case T.breakOn " :" (dropColon txt) of
    (x,y) | T.null y ->
              T.words txt
          | otherwise ->
              let (_, msg) = T.break (== ':') y
              in T.words x ++ [msg]

takeLast :: Text -> Text
takeLast xs = T.take (T.length xs - 1) xs

takeCarriageRet :: Text -> Text
takeCarriageRet xs =
  if T.drop (T.length xs - 1) xs == "\r"
    then takeLast xs
    else xs

dropColon :: Text -> Text
dropColon xs =
  if T.take 1 xs == ":"
    then T.drop 1 xs
    else xs

showCommand :: Command -> Text
showCommand (MPrivmsg chan msg)             = "PRIVMSG " `T.append` chan `T.append` " :" `T.append` msg
showCommand (MJoin    chan (Just key))      = "JOIN "    `T.append` chan `T.append` " "  `T.append` key
showCommand (MJoin    chan Nothing)         = "JOIN "    `T.append` chan
showCommand (MPart    chan msg)             = "PART "    `T.append` chan `T.append` " :" `T.append` msg
showCommand (MMode    chan mode (Just usr)) = "MODE "    `T.append` chan `T.append` " "  `T.append` mode `T.append` " " `T.append` usr
showCommand (MMode    chan mode Nothing)    = "MODE "    `T.append` chan `T.append` " "  `T.append` mode
showCommand (MTopic   chan (Just msg))      = "TOPIC "   `T.append` chan `T.append` " :" `T.append` msg
showCommand (MTopic   chan Nothing)         = "TOPIC "   `T.append` chan
showCommand (MInvite  usr chan)             = "INVITE "  `T.append` usr  `T.append` " "  `T.append` chan
showCommand (MKick    chan usr msg)         = "KICK "    `T.append` chan `T.append` " "  `T.append` usr  `T.append` " :" `T.append` msg
showCommand (MQuit    msg)                  = "QUIT :"   `T.append` msg
showCommand (MNick    nick)                 = "NICK "    `T.append` nick
showCommand (MNotice  chan msg)             = "NOTICE "  `T.append` chan `T.append` " :" `T.append` msg
showCommand (MAction  chan msg)             = showCommand $ MPrivmsg chan ("\x01ACTION " `T.append` msg  `T.append` "\x01")
showCommand (MUser    user mode real)       = "USER "    `T.append` user `T.append` " "  `T.append` mode `T.append` " * " `T.append` " :" `T.append` real
