{-
-- EPITECH PROJECT, 2022
-- GLaDOS
-- File description:
-- Unit tests
-}

import Test.HUnit

import Control.Monad (void)
import Parse

tests :: Test
tests =
    TestList
        [ testParse
        ]

main :: IO ()
main = void $ runTestTT tests
