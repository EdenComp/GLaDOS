module Unit.Dreamberd.TestDreamberdParsing (testDreamberdParsing) where

import Dreamberd.Types (AstNode (Boolean, Call, Function, Identifier, If, Loop, Number, Return, String), File (File))
import Dreamberd.Parser (Parser, parseChar, parseAnyChar, parseAndWith, parseMany, parseSome, parse, parseDreamberd, parseNumber)
import Test.HUnit (Test (..), assertEqual, assertBool)
import Data.Either (isLeft)

testDreamberdParsing :: Test
testDreamberdParsing = TestList [testParseNumber, testParseDreamberd, testParseAndWith, testParseMany]


testParseNumber :: Test
testParseNumber =
    TestList [ 
        TestCase (assertEqual "for (parseNumber \"123\")" 
            (Right (Number 123, ("", "", (1, 4)))) 
            (parse parseNumber ("", "123", (1, 1))))
    ]

testParseAndWith :: Test
testParseAndWith = TestList [
    TestCase (assertEqual "for (parseAndWith (,) (parseChar 'a') (parseChar 'a') on \"aa\")" 
        (Right (('a', 'a'), ("", "", (1, 3)))) 
        (parse (parseAndWith (,) (parseChar 'a') (parseChar 'a')) ("", "aa", (1, 1)))),
    TestCase (assertBool "for failure case of parseAndWith" 
        (case parse (parseAndWith (,) (parseChar 'a') (parseChar 'a')) ("", "ab", (1, 1)) of
            Left _ -> True
            Right _ -> False))
    ]

testParseMany :: Test
testParseMany = 
    TestList [TestCase (assertEqual "for (parseMany (parseChar 'a') on \"aaa\")" 
        (Right ("aaa", ("", "", (1, 4)))) 
        (parse (parseMany (parseChar 'a')) ("", "aaa", (1, 1))))
        ,TestCase (assertEqual "for (parseMany (parseChar 'a') on empty string)"  (Right ("", ("", "", (1, 1)))) (parse (parseMany (parseChar 'a')) ("", "", (1, 1))))
    ]

testParseDreamberd :: Test
testParseDreamberd =
    TestList [TestCase (assertEqual "parseDreamberd basic assign var"
        (Right [Call "=" [Identifier "int", Identifier "a", Number 1]])
        (parseDreamberd (File "" "int a = 1;")))
    ]


