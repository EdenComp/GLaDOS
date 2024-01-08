module Unit.Dreamberd.TestDreamberdParsing (testDreamberdParsing) where

import Dreamberd.Parsing.Main (parseCondition, parseFunction)
import Dreamberd.Parsing.Values (parseFunctionCall)
import Dreamberd.Types (AstNode (Boolean, Call, Function, Identifier, If, Number, Operator, Return, String))
import Test.HUnit (Test (..), assertEqual)

testDreamberdParsing :: Test
testDreamberdParsing = TestList [testParseFunction, testParseFunctionCall, testParseCondition]

testParseFunction :: Test
testParseFunction =
    TestList
        [ TestCase (assertEqual "parseFunction basic" (Right ("", [Function "foo" [] [Return (Number 1)]])) (parseFunction "foo(){return 1;}" []))
        , TestCase (assertEqual "parseFunction nested" (Right ("", [Function "foo" [] [Function "bar" [] [Operator "=" (String "b") (Boolean True)], Return (Number 1)]])) (parseFunction "foo(){ function bar() {bool b = true;} return 1;}" []))
        , TestCase (assertEqual "parseFunction with params" (Right ("", [Function "foo" ["bar"] [Return (Number 1)]])) (parseFunction "foo(bar){return 1;}" []))
        , TestCase (assertEqual "parseFunction with params and body" (Right ("", [Function "foo" ["bar", "other"] [Return (Number 1)]])) (parseFunction "foo(bar, other ){return 1;}" []))
        , TestCase (assertEqual "parseFunction with params, body and spaces" (Right ("", [Function "foo" ["bar"] [Return (Number 1)]])) (parseFunction "foo ( bar ) { return 1; }" []))
        , TestCase (assertEqual "parseFunction with params, body, spaces and newlines" (Right ("", [Function "foo" ["bar"] [Return (Number 1)]])) (parseFunction "foo ( bar ) {\nreturn 1;\n}" []))
        , TestCase (assertEqual "parseFunction with multi-content" (Right ("", [Function "foo" ["bar"] [Operator "=" (String "a") (Number 42), Operator "=" (String "b") (Identifier "bar"), Return (Identifier "bar")]])) (parseFunction "foo ( bar ) {\nint a = 42; int b = bar; return bar;\n}" []))
        , TestCase (assertEqual "parseFunction wrong - no parenthesis" (Left "No parenthesis found for function params") (parseFunction "foo{return bar; }" []))
        , TestCase (assertEqual "parseFunction wrong - only open parenthesis" (Left "No parenthesis found for function params") (parseFunction "foo({return bar; }" []))
        , TestCase (assertEqual "parseFunction wrong - only close parenthesis" (Left "No parenthesis found for function params") (parseFunction "foo){return bar; }" []))
        , TestCase (assertEqual "parseFunction wrong - invalid function name" (Left "Variable name 'int' is banned") (parseFunction "int(){return bar; }" []))
        , TestCase (assertEqual "parseFunction wrong - invalid function scope" (Left "Unrecognized element") (parseFunction "foo(){unknown content; }" []))
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

testParseCondition :: Test
testParseCondition =
    TestList
        [ TestCase (assertEqual "parseCondition basic" (Right ("", [If (Boolean True) [Return (Number 1)] []])) (parseCondition "if (true) {return 1;}" []))
        , TestCase (assertEqual "parseCondition with else" (Right ("", [If (Boolean True) [Return (Number 1)] [Return (Number 2)]])) (parseCondition "if (true) {return 1;} else {return 2;}" []))
        , TestCase (assertEqual "parseCondition with elif" (Right ("", [If (Boolean True) [Return (Number 1)] [If (Boolean True) [Return (Number 2)] []]])) (parseCondition "if (true) {return 1;} elif (true) {return 2;}" []))
        , TestCase (assertEqual "parseCondition with elif and else" (Right ("", [If (Boolean True) [Return (Number 1)] [If (Boolean True) [Return (Number 2)] [Return (Number 3)]]])) (parseCondition "if (true) {return 1;} elif (true) {return 2;} else {return 3;}" []))
        , TestCase (assertEqual "parseCondition with elif, else and spaces" (Right ("", [If (Boolean True) [Return (Number 1)] [If (Boolean True) [Return (Number 2)] [Return (Number 3)]]])) (parseCondition "if (true) {return 1;} elif (true) {return 2;} else {return 3;}" []))
        , TestCase (assertEqual "parseCondition with elif, else, spaces and newlines" (Right ("", [If (Boolean True) [Return (Number 1)] [If (Boolean True) [Return (Number 2)] [Return (Number 3)]]])) (parseCondition "if (true) {\nreturn 1;\n} elif (true) {\nreturn 2;\n} else {\nreturn 3;\n}" []))
        ]
