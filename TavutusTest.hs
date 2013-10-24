{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- File:          
-- Creation Date:
-- Last Modified: Oct 25 2013 [02:10:20]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

import Tavutus
import Data.Monoid
import Control.Applicative
import Test.HUnit

exsuc :: (String, Runo) -> Test
exsuc (s, expected) = TestCase
    $ either (\er -> assertFailure $
                "ParseError on " ++ s ++ ": " ++ show er)
             (\calc -> if expected == calc
                        then return ()
                        else assertFailure $ "=== " ++ s ++ " === Expected: "
                            ++ show expected ++ ".  Parsed: "
                            ++ show calc)
    $ tavutaRuno s

exact = [ ("a o e i u y ö ä"
          , [[["a"],["o"],["e"],["i"],["u"],["y"],["ö"],["ä"]]] )

        -- Weak diphthongs begin
        , ("mieti",     [[["mie", "ti"     ]]])
        , ("nuotio",    [[["nuo", "ti", "o"]]])
        , ("yössä",     [[["yös", "sä"     ]]])
        , ("suomuin",   [[["suo", "muin"   ]]])

        -- Weak diphtongs not begin
        , ("värien",      [[["vä", "ri", "en"  ]]])
        , ("nuottien",    [[["nuot", "ti", "en"]]])

        -- Weak diphtong begins a compound work
        , ("katovuosi",   [[["ka", "to", "vuo", "si"   ]]])
        , ("asfalttitie", [[["as", "falt", "ti", "tie" ]]])

        -- Poem separator
        , (""  ,   [ [] ])
        , (";" ,   [ [],[] ])
        , (";;",   [ [],[] ])
        , (" ;; ", [ [],[] ])
        , ("a;",   [ [["a"]], [] ])
        , (";;a",  [ [], [["a"]] ])
        , (";a;",  [ [], [["a"]], [] ])

        , ( "eka ; toka ;;; kolmas"
          , [ [["e","ka"]],[["to","ka"]],[["kol","mas"]]] )
        ]

tests = TestList $
    [ TestLabel "Exact" $ TestList (map exsuc exact) ]

main = runTestTT tests
