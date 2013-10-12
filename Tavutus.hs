------------------------------------------------------------------------------
-- File:          Tavutus.hs
-- Creation Date: Jul 06 2012
-- Last Modified: Oct 10 2013 [00:14:49]
-- Created By :   Samuli Thomasson [SimSaladin] samuli.thomassonATgmail.com
------------------------------------------------------------------------------

module Tavutus where

-- 1. Konsonanttisääntö: Jos tavuun kuuluvaa vokaalia seuraa yksi tai useampia
--    konsonantteja,joita vielä seuraa vokaali,tavuraja sijoittuu välittömästi
--    ennen viimeistä konsonanttia.
--
-- 2. Vokaalisääntö: Jos tavun ensimmäistä vokaalia seuraa toinen vokaali,
--    niiden väliin tulee tavuraja, ellei
--    a) edellinen vokaali ole sama kuin jälkimmäinen (pitkä vokaali).
--    b) jälkimmäinen vokaali ole i (i:hin loppuva diftongi).
--    c) kysymyksessä ole jokin vokaalipareista au,eu,ie,iu,ou,uo,yö,äy,
--    öy,ey tai iy (muu diftongi).
--
-- 3. Diftongisääntö: Jos tavun kuuluvaa diftongi tai pitkää vokaalia seuraa
-- vokaali,tähän väliin tulee aina tavuraja.
--
-- 4. Poikkeussääntö: Yhdyssanat jaetaan tavuihin sanojen välistä,myös siinä
-- tapauksessa,että sana on yhdyssana vain alkuperäiskielessä.
--
-- references: http://teppo.tv/haikueditori/tavutus.html
--             http://fi.wikipedia.org/wiki/Tavu
--
-- Update (10/2013)
--
--   + Diftongit 1. tavussa: kaikki

import Text.ParserCombinators.Parsec
--import Data.Monoid
import Control.Applicative hiding ((<|>), many)
import Data.List (delete)
import qualified Data.List as L
import Data.Char (toUpper)

import Debug.Trace

type Tavu  = String
type Sana  = [Tavu]
type Sae   = [Sana]
type Runo  = [Sae]

-- * Entry points

-- | Säkeet erotetaan ";":lla tai "/". tai "//":lla.
tavutaRuno :: String -> Either ParseError Runo
tavutaRuno input = (delete []) <$> parse runo "" ((dropWhile (== ' ') input) ++ " ")

-- | Tavutus PP.
printTavut :: Runo -> String
printTavut = L.intercalate " // " . map (L.intercalate " " . map (L.intercalate "-"))

-- * Chars, Tavut

kons, voks          :: [Char]
kons = "BCDFGHJKLMNPRSTVZXbcdfghjklmnŋprsštvzžx"
voks = "AEIOUYÄÖaeiouyäö"

kon,  vok           :: Parser Char
kon  = oneOf kons
vok  = oneOf voks

vok2, dft, dft2, puncts :: Parser String
vok2 = choice $ map (try . string) $ flip concatMap "aeiouyäö"
    (\x -> let y = toUpper x in [x, x] : [x, y] : [y, x] : [[y,y]])

f_dft :: [Tavu] -> Parser String
f_dft = choice . map (try . string) . concatMap
    (\[a, b] -> let f = toUpper in [f a,b]:[f a,f b]:[a,f b]:[[a, b]])

dft  = f_dft [ "ai","ei","oi","ui","yi","äi","öi"
             , "au","eu","iu","ou","ie","uo","yö"
             , "äy","öy","ey","iy" ]

-- | version of diftongs on non-first syllables; excludes ie uo yö.
dft2 = f_dft [ "ai","ei","oi","ui","yi","äi","öi"
             , "au","eu","iu","ou"
             , "äy","öy","ey","iy" ]

puncts = many . noneOf $ kons ++ voks ++ " ;/"

-- * Parsers

runo :: Parser Runo
runo = sepEndBy sae $
    many1 $ many space >> many1 (char ';' <|> char '/') >> many space

sae :: Parser Sae
sae = sepEndBy sana (many1 space)

sana :: Parser Sana
sana = (\x xs -> x : xs) <$> ekaTavu
                         <*> many jatkoTavu

ekaTavu, jatkoTavu :: Parser String
ekaTavu = (\p k v k' p' -> p ++ k ++ v ++ k' ++ p')
    <$> puncts <*> many kon
    <*> choice [dft, vok2, fmap return vok]
    <*> choice [endsKon, initKons ]
    <*> puncts

jatkoTavu = (\p k v k' p' -> p ++ k ++ v ++ k' ++ p')
    <$> puncts <*> many kon
    <*> choice [dft2, vok2, fmap return vok]
    <*> choice [endsKon, initKons ]
    <*> puncts


endsKon, initKons :: Parser String
endsKon  =        try $ many kon <* (lookAhead . noneOf $ kons ++ voks)
initKons = many . try $      kon <* (lookAhead $ kon >> choice [try kon, vok]) 
