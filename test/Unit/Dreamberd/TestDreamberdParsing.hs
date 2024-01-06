module Unit.Dreamberd.TestDreamberdParsing (testDreamberdParsing) where

import Dreamberd.Parsing.Main (parseFunction)
import Dreamberd.Parsing.Values (parseFunctionCall)
import Dreamberd.Types (AstNode (Boolean, Call, Function, Identifier, Number, Operator, Return, String))
import Test.HUnit (Test (..), assertEqual)

testDreamberdParsing :: Test
testDreamberdParsing = TestList [testParseFunction, testParseFunctionCall]

testParseFunction :: Test
testParseFunction =
    TestList
        [ TestCase (assertEqual "parseFunction basic" (Right ("", [Function "foo" [] [Return (Number 1)]])) (parseFunction "foo(){return 1;}" []))
        , TestCase (assertEqual "parseFunction with params" (Right ("", [Function "foo" ["bar"] [Return (Number 1)]])) (parseFunction "foo(bar){return 1;}" []))
        , TestCase (assertEqual "parseFunction with params and body" (Right ("", [Function "foo" ["bar"] [Return (Number 1)]])) (parseFunction "foo(bar){return 1;}" []))
        , TestCase (assertEqual "parseFunction with params, body and spaces" (Right ("", [Function "foo" ["bar"] [Return (Number 1)]])) (parseFunction "foo ( bar ) { return 1; }" []))
        , TestCase (assertEqual "parseFunction with params, body, spaces and newlines" (Right ("", [Function "foo" ["bar"] [Return (Number 1)]])) (parseFunction "foo ( bar ) {\nreturn 1;\n}" []))
        , TestCase (assertEqual "parseFunction with multi-content" (Right ("", [Function "foo" ["bar"] [Operator "=" (String "a") (Number 42), Operator "=" (String "b") (Identifier "bar"), Return (Identifier "bar")]])) (parseFunction "foo ( bar ) {\nint a = 42; int b = bar; return bar;\n}" []))
        ]

testParseFunctionCall :: Test
testParseFunctionCall =
    TestList
        [ TestCase (assertEqual "parseFunctionCall basic" (Right ("", Call "foo" [])) (parseFunctionCall "foo();"))
        , TestCase (assertEqual "parseFunctionCall with param String" (Right ("", Call "foo" [String "bar"])) (parseFunctionCall "foo(\"bar\");"))
        , TestCase (assertEqual "parseFunctionCall with params Number" (Right ("", Call "foo" [Number 1])) (parseFunctionCall "foo ( 1 );"))
        , TestCase (assertEqual "parseFunctionCall with params Boolean" (Right ("", Call "foo" [Boolean True])) (parseFunctionCall "foo ( true )\n;"))
        , TestCase (assertEqual "parseFunctionCall with multi-params" (Right ("", Call "foo" [Number 1, Number 2])) (parseFunctionCall "foo ( 1 , 2 );"))
        ]
