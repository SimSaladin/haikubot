{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
-- Finnish syllabification.
--
--
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
module Tavutus where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), many)
import qualified Data.List as L
import Data.Char (toUpper)
import Data.String (IsString)
import Data.Maybe

-- * Entry points

-- | Säkeet erotetaan \";\":lla tai \"/\". tai \"//\":lla.
tavutaRuno :: String -> Either ParseError Runo
tavutaRuno input = parse runo "" input

-- | Tavutus PP.
printTavut :: Runo -> String
printTavut = f " // " . f " " $ f "-" unTavu
        where
            f x g = L.intercalate x . map g

-- * Implementation (Finnish)

newtype Tavu = Tavu { unTavu :: [Char] } deriving (Eq, IsString)
type Sana    = [Tavu]
type Sae     = [Sana]
type Runo    = [Sae]

tavu :: Parser String -> Parser Tavu
tavu = fmap Tavu

instance Show Tavu where
    show (Tavu xs) = xs

-- ** Characters

consonants, vowels', vowels :: [Char]
consonants = "BCDFGHJKLMNPRSTVZXbcdfghjklmnŋprsštvzžx"
vowels' = "aeiouyäö"
vowels  = (++) <*> map toUpper $ vowels'

weak_diphs, strong_diphs :: [String]
weak_diphs   = ["ie","uo","yö"]
strong_diphs = ["ai","ei","oi","ui","yi","äi","öi","au","eu","iu","ou", {- weak -} "äy","öy","ey","iy"]

-- ** Parsers

vowel, consonant :: Parser Char
consonant = oneOf consonants
vowel     = oneOf vowels

strong_diph, weak_diph, any_diph :: Parser String
(strong_diph, weak_diph, any_diph) = ( choiceTavu strong_diphs
                                     , choiceTavu weak_diphs
                                     , choiceTavu $ strong_diphs ++ weak_diphs )

-- | aa, ee, ii, ...
double_vowel :: Parser String
double_vowel = choiceTavu $ replicate 2 `map` vowels

-- | Something else than a (known) vowel, consonant or reserved (\";\" and \"/\").
punct_mark :: Parser Char
punct_mark = noneOf $ consonants ++ vowels ++ " ;/"

-- | 
choiceTavu :: [String] -> Parser String
choiceTavu = choice . map (try . string) . concatMap caseAlt

caseAlt :: String -> [String]
caseAlt [a, b] = [[f a, b], [f a, f b], [a, f b], [a, b]] where f = toUpper
caseAlt      _ = error "Unhandled input"

-- * Parsers

runo :: Parser Runo
runo = sepEndBy sae . many1 $
    between (many space) (many space) $ many1 (char ';' <|> char '/' <?> "a säe")

sae :: Parser Sae
sae = catMaybes <$> sepEndBy sanaLike (many1 space)
    where
        sanaLike = (Just <$> try sana) <|>
                   (many1 punct_mark >> return Nothing) <?> "sana"

sana :: Parser Sana
sana = concat <$> sequence
        [ pure <$> tavuFirst
        , (concat <$>) . many . try $ sequence [tavuNth, tavuFirst]
        , many tavuNth ]

pair :: Parser Tavu -> Parser Tavu -> Parser Sana
pair x y = sequence [x, y]

tavuPre :: Parser String -> Parser Tavu
tavuPre diph = tavu $ concat <$> sequence
        [ many punct_mark
        , many consonant
        , choice [diph, double_vowel, pure <$> vowel] <?> "a letter"
        , choice [consonantEnd, consonantInit]
        , many punct_mark ]

tavuFirst, tavuNth :: Parser Tavu
tavuFirst = tavuPre any_diph
tavuNth   = tavuPre strong_diph

-- | If only consonants ahead (no vowels) => consume them all.
consonantEnd :: Parser String
consonantEnd  = try $ many consonant <* lookAhead eow
    where eow = choice [noneOf (vowels ++ consonants) >> return (), eof]

-- | Of n (>= 0) consonants ahead, consume the first n-1.
consonantInit :: Parser String
consonantInit = many $ try (consonant <* lookAhead twoConAhead)
    where twoConAhead = consonant >> choice [try consonant, vowel]

