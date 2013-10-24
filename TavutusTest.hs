{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- File:          
-- Creation Date:
-- Last Modified: Oct 25 2013 [01:12:56]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

import Tavutus
import Data.Monoid
import Control.Applicative
import Test.HUnit

psuc :: String -> Test
psuc s = TestCase $ case tavutaRuno s of
    Right _ -> return ()
    Left er -> assertFailure $
        "ParseError when expecting success on " <> s <> ": " <> show er

exsuc :: (String, [Tavu]) -> Test
exsuc (s, expected) = TestCase
    $ either (\er -> assertFailure $ "ParseError on " ++ s ++ ": " ++ show er)
             (\calc -> if expected == calc
                        then return ()
                        else assertFailure $ "Expected: "
                            ++ show expected ++ ".  Parsed: "
                            ++ show calc)
    $ (concat . concat) <$> tavutaRuno s

successes = [ "" ]

exact = [ ("a o e i u y ö ä", ["a","o","e","i","u","y","ö","ä"])

        -- Weak diphthongs begin
        , ("mieti",  ["mie", "ti"])
        , ("nuotio", ["nuo", "ti", "o"])
        , ("yössä",  ["yös", "sä"])
        , ("suomuin", ["suo", "muin"])

        -- Weak diphtongs not begin
        , ("värien",      ["vä", "ri", "en"])
        , ("nuottien",    ["nuot", "ti", "en"])

        -- Weak diphtong begins a compound work
        , ("katovuosi",   ["ka", "to", "vuo", "si"])
        , ("asfalttitie", ["as","falt","ti","tie"])
        ]

tests = TestList $
    [ TestLabel "Successes" $ TestList (map psuc successes)
    , TestLabel "Exact" $ TestList (map exsuc exact)
    ]

main = runTestTT tests
