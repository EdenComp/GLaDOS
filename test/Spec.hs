{-
-- EPITECH PROJECT, 2022
-- GLaDOS
-- File description:
-- Unit tests
-}

import Test.HUnit

import Parse

tests :: Test
tests = TestList [
    testParse
    ]

main :: IO ()
main = runTestTT tests >> return ()
