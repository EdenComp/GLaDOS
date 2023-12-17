module Parse (testParse) where

import Parsing
import Test.HUnit

testParseChar :: Test
testParseChar =
    TestList
        [ TestCase (assertEqual "parseChar 'a' \"abc\"" (Just ('a', "bc")) (parse (parseChar 'a') "abc"))
        , TestCase (assertEqual "parseChar 'a' \"bcd\"" Nothing (parse (parseChar 'a') "bcd"))
        , TestCase (assertEqual "parseChar 'a' \"\"" Nothing (parse (parseChar 'a') ""))
        , TestCase (assertEqual "parseChar 'a' \"a\"" (Just ('a', "")) (parse (parseChar 'a') "a"))
        ]

testParseAnyChar :: Test
testParseAnyChar =
    TestList
        [ TestCase (assertEqual "parseAnyChar \"abc\" \"abc\"" (Just ('a', "bc")) (parse (parseAnyChar "abc") "abc"))
        , TestCase (assertEqual "parseAnyChar \"abc\" \"bcd\"" (Just ('b', "cd")) (parse (parseAnyChar "abc") "bcd"))
        , TestCase (assertEqual "parseAnyChar \"abc\" \"\"" Nothing (parse (parseAnyChar "abc") ""))
        , TestCase (assertEqual "parseAnyChar \"abc\" \"d\"" Nothing (parse (parseAnyChar "abc") "d"))
        ]

testParseOr :: Test
testParseOr =
    TestList
        [ TestCase (assertEqual "parseOr (parseChar 'a') (parseChar 'b') \"abc\"" (Just ('a', "bc")) (parse (parseOr (parseChar 'a') (parseChar 'b')) "abc"))
        , TestCase (assertEqual "parseOr (parseChar 'a') (parseChar 'b') \"bcd\"" (Just ('b', "cd")) (parse (parseOr (parseChar 'a') (parseChar 'b')) "bcd"))
        , TestCase (assertEqual "parseOr (parseChar 'a') (parseChar 'b') \"\"" Nothing (parse (parseOr (parseChar 'a') (parseChar 'b')) ""))
        , TestCase (assertEqual "parseOr (parseChar 'a') (parseChar 'b') \"c\"" Nothing (parse (parseOr (parseChar 'a') (parseChar 'b')) "c"))
        ]

testParseAnd :: Test
testParseAnd =
    TestList
        [ TestCase (assertEqual "parseAnd (parseChar 'a') (parseChar 'b') \"abc\"" (Just (('a', 'b'), "c")) (parse (parseAnd (parseChar 'a') (parseChar 'b')) "abc"))
        , TestCase (assertEqual "parseAnd (parseChar 'a') (parseChar 'b') \"bcd\"" Nothing (parse (parseAnd (parseChar 'a') (parseChar 'b')) "bcd"))
        , TestCase (assertEqual "parseAnd (parseChar 'a') (parseChar 'b') \"\"" Nothing (parse (parseAnd (parseChar 'a') (parseChar 'b')) ""))
        , TestCase (assertEqual "parseAnd (parseChar 'a') (parseChar 'b') \"c\"" Nothing (parse (parseAnd (parseChar 'a') (parseChar 'b')) "c"))
        ]

testParseAndWith :: Test
testParseAndWith =
    TestList
        [ TestCase (assertEqual "parseAndWith (\\x y -> [x, y]) (parseChar 'a') (parseChar 'b') \"abc\"" (Just ("ab", "c")) (parse (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'b')) "abc"))
        , TestCase (assertEqual "parseAndWith (\\x y -> [x, y]) (parseChar 'a') (parseChar 'b') \"bcd\"" Nothing (parse (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'b')) "bcd"))
        , TestCase (assertEqual "parseAndWith (\\x y -> [x, y]) (parseChar 'a') (parseChar 'b') \"\"" Nothing (parse (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'b')) ""))
        , TestCase (assertEqual "parseAndWith (\\x y -> [x, y]) (parseChar 'a') (parseChar 'b') \"c\"" Nothing (parse (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'b')) "c"))
        ]

testParseMany :: Test
testParseMany =
    TestList
        [ TestCase (assertEqual "parseMany (parseChar 'a') \"abc\"" (Just ("a", "bc")) (parse (parseMany (parseChar 'a')) "abc"))
        , TestCase (assertEqual "parseMany (parseChar 'a') \"bcd\"" (Just ("", "bcd")) (parse (parseMany (parseChar 'a')) "bcd"))
        , TestCase (assertEqual "parseMany (parseChar 'a') \"\"" (Just ("", "")) (parse (parseMany (parseChar 'a')) ""))
        , TestCase (assertEqual "parseMany (parseChar 'a') \"c\"" (Just ("", "c")) (parse (parseMany (parseChar 'a')) "c"))
        ]

testParseSome :: Test
testParseSome =
    TestList
        [ TestCase (assertEqual "parseSome (parseChar 'a') \"abc\"" (Just ("a", "bc")) (parse (parseSome (parseChar 'a')) "abc"))
        , TestCase (assertEqual "parseSome (parseChar 'a') \"bcd\"" Nothing (parse (parseSome (parseChar 'a')) "bcd"))
        , TestCase (assertEqual "parseSome (parseChar 'a') \"\"" Nothing (parse (parseSome (parseChar 'a')) ""))
        , TestCase (assertEqual "parseSome (parseChar 'a') \"c\"" Nothing (parse (parseSome (parseChar 'a')) "c"))
        ]

testParseUInt :: Test
testParseUInt =
    TestList
        [ TestCase (assertEqual "parseUInt \"123\"" (Just (123, "")) (parse parseUInt "123"))
        , TestCase (assertEqual "parseUInt \"-123\"" Nothing (parse parseUInt "-123"))
        , TestCase (assertEqual "parseUInt \"abc\"" Nothing (parse parseUInt "abc"))
        , TestCase (assertEqual "parseUInt \"\"" Nothing (parse parseUInt ""))
        ]

testParseInt :: Test
testParseInt =
    TestList
        [ TestCase (assertEqual "parseInt \"123\"" (Just (123, "")) (parse parseInt "123"))
        , TestCase (assertEqual "parseInt \"-123\"" (Just (-123, "")) (parse parseInt "-123"))
        , TestCase (assertEqual "parseInt \"abc\"" Nothing (parse parseInt "abc"))
        , TestCase (assertEqual "parseInt \"\"" Nothing (parse parseInt ""))
        ]

testParsePair :: Test
testParsePair =
    TestList
        [ TestCase
            ( assertEqual
                "parsePair parseInt \"(123 456) foo bar \""
                (Just ((123, 456), " foo bar "))
                (parse (parsePair parseInt) "(123 456) foo bar ")
            )
        , TestCase
            ( assertEqual
                "parsePair parseInt \"(-123 456)\""
                (Just ((-123, 456), ""))
                (parse (parsePair parseInt) "(-123 456)")
            )
        , TestCase
            ( assertEqual
                "parsePair parseInt \"(abc 123)\""
                Nothing
                (parse (parsePair parseInt) "(abc 123)")
            )
        , TestCase
            ( assertEqual
                "parsePair parseInt \"(123 abc)\""
                Nothing
                (parse (parsePair parseInt) "(123 abc)")
            )
        , TestCase
            ( assertEqual
                "parsePair parseInt \"(123)\""
                Nothing
                (parse (parsePair parseInt) "(123)")
            )
        , TestCase
            ( assertEqual
                "parsePair parseInt \"(123 456 789)\""
                Nothing
                (parse (parsePair parseInt) "(123 456 789)")
            )
        ]

testParse :: Test
testParse =
    TestList
        [ TestLabel "parseChar" testParseChar
        , TestLabel "parseAnyChar" testParseAnyChar
        , TestLabel "parseOr" testParseOr
        , TestLabel "parseAnd" testParseAnd
        , TestLabel "parseAndWith" testParseAndWith
        , TestLabel "parseMany" testParseMany
        , TestLabel "parseSome" testParseSome
        , TestLabel "parseUInt" testParseUInt
        , TestLabel "parseInt" testParseInt
        , TestLabel "parsePair" testParsePair
        ]
