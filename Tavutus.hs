------------------------------------------------------------------------------
-- File:          Tavutus.hs
-- Creation Date: Jul 06 2012
-- Last Modified: Jul 17 2012 [18:46:07]
-- Created By :   Samuli Thomasson [SimSaladin] samuli.thomassonATgmail.com
------------------------------------------------------------------------------

module Tavutus where

-- 1. Konsonanttisääntö: Jos tavuun kuuluvaa vokaalia seuraa yksi tai useampia
--    konsonantteja,joita vielä seuraa vokaali,tavuraja sijoittuu välittömästi
--    ennen viimeistä konsonanttia.
--
-- 2. Vokaalisääntö: Jos tavun ensimmäistä vokaalia seuraa toinen vokaali,
--    niiden väliin tulee tavuraja,ellei
--    a) edellinen vokaali ole sama kuin jälkimmäinen (pitkä vokaali).
--    b) jälkimmäinen vokaali ole i (i:hin loppuva diftongi).
--    c) kysymyksessä ole jokin vokaalipareista au,eu,ie,iu,ou,uo,yö,äy,
--    öy,ey tai iy (muu diftongi).
--
-- 3. Diftongisääntö: Jos tavun kuuluvaa difta tai pitkää vokaalia seuraa
-- vokaali,tähän väliin tulee aina tavuraja.
--
-- 4. Poikkeussääntö: Yhdyssanat jaetaan tavuihin sanojen välistä,myös siinä
-- tapauksessa,että sana on yhdyssana vain alkuperäiskielessä.
--
-- references: http://teppo.tv/haikueditori/tavutus.html
--             http://fi.wikipedia.org/wiki/Tavu
--

import Text.ParserCombinators.Parsec
import Data.List (delete)
import Data.Char (toUpper)

kons = "BCDFGHJKLMNPRSTVZXbcdfghjklmnŋprsštvzžx"
voks = "AEIOUYÄÖaeiouyäö"

printTavut :: [[[String]]] -> String
printTavut s = foldl1 (\x y -> x ++ "; " ++ y) (map psae s)
   where
      psae :: [[String]] -> String
      psae []  = ""
      psae sae = foldl1 (\x y -> x ++ " " ++ y) (map psana sae)

      psana :: [String] -> String
      psana []   = ""
      psana sana = foldl1 (\x y -> x ++ "-" ++ y) sana

kon = oneOf kons
vok = oneOf voks
dift = foldl1 (<|>) $ map (try . string) $ concatMap
   (\[a,b] -> [toUpper a:[b], toUpper a:[toUpper b], a:[toUpper b]])
   [ "ai","ei","oi","ui","yi","äi","öi"
   , "au","eu","iu","ou","ie","uo","yö"
   , "äy","öy","ey","iy"
   ]

vokDouble = foldl1 (<|>) (map (\x -> string (x:[x])) voks)

tavutaRuno :: String -> Either String [[[String]]]
tavutaRuno input = case parse parseRuno "" ((dropWhile (== ' ') input) ++ " ") of
   Left err -> Left ("Osaatko edes kirjoittaa? ("++show err++")")
   Right val -> Right $ delete [] val

parseRuno :: Parser [[[String]]]
parseRuno = sepEndBy parseTavutaSanat (many1 (many space >> many1 (char ';') >> many space))

parseTavutaSanat :: Parser [[String]]
parseTavutaSanat = sepEndBy (many1 parseTavu) (many1 space)

parseTavu :: Parser String
parseTavu = do
   puncts' <- many (noneOf (kons ++ voks ++ " ;"))
   ks <- many kon         
   v  <- try dift <|> try vokDouble <|> fmap return vok 
   ls <- try loppuuKon <|> many (try (kon `followedByY` (kon >> (try kon <|> vok))))
   puncts <- many (noneOf (kons ++ voks ++ " ;"))
   return (puncts' ++ ks ++ v ++ ls ++ puncts)

loppuuKon :: Parser String
loppuuKon = many kon >>= \r -> try $ lookAhead (noneOf (kons ++ voks)) >> return r

followedByY :: Parser Char -> Parser Char -> Parser Char
followedByY x y = x >>= \r -> lookAhead y >> return r

