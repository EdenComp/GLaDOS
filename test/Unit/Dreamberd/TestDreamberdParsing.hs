module Unit.Dreamberd.TestDreamberdParsing (testDreamberdParsing) where

import Dreamberd.Parsing.Elements.Condition (parseConditionExpression)
import Dreamberd.Parsing.Main (parseCondition, parseFunction, parseLoop)
import Dreamberd.Parsing.Values (parseFunctionCall, parseOperatorValue)
import Dreamberd.Types (AstNode (Boolean, Call, Function, Identifier, If, Loop, Number, Return, String))
import Dreamberd.Parsing.Parser (Parser, parseChar, parseAnyChar, parseAnd, parseAndWith, parseMany, parseSome, parseIf, parseNumber, parser, parseDreamberd)
import Test.HUnit (Test (..), assertEqual, assertBool)
import Data.Either (isLeft)

testDreamberdParsing :: Test
testDreamberdParsing = TestList [testParseFunction, testParseFunctionCall, testParseCondition, testParseOperatorValue, testParseConditionExpression, testParseLoop, testParseDreamberd, testParseNumber]


testParseNumber :: Test
testParseNumber =
    TestList[TestCase (assertEqual "for (parseNumber \"123\")" (Right (Number 123, ("", 3))) (parser parseNumber ("123", 0)))
        , TestCase (assertEqual "for (parseNumber \"-123\")" (Right (Number (-123), ("", 4))) (parser parseNumber ("-123", 0)))
        , TestCase (assertEqual "for (parseNumber \"abc\")" (Left "Expected '-' but found 'a' at 0") (parser parseNumber ("abc", 0)))
        ]

testParseAnd :: Test
testParseAnd =
    TestList[TestCase (assertEqual "for (parseAnd (parseChar 'a') (parseChar 'b'))" (Right (('a', 'b'), ("", 2))) (parser (parseAnd (parseChar 'a') (parseChar 'b')) ("ab", 0)))
        , TestCase (assertEqual "for (parseAnd (parseChar 'a') (parseChar 'b'))" (Left "Expected 'b' but found 'a' at 0") (parser (parseAnd (parseChar 'a') (parseChar 'b')) ("aa", 0)))
        , TestCase (assertEqual "for (parseAnd (parseChar 'a') (parseChar 'b'))" (Left "Expected 'a' but found end of file at 0") (parser (parseAnd (parseChar 'a') (parseChar 'b')) ("b", 0)))
        ]

testParseAndWith :: Test
testParseAndWith = TestList [
    TestCase (assertEqual "for (parseAndWith (,) (parseChar 'a') (parseChar 'a') on \"aa\")" 
              (Right (('a', 'a'), ("", 2))) 
              (parser (parseAndWith (,) (parseChar 'a') (parseChar 'a')) ("aa", 0))),
    TestCase (assertBool "for failure case of parseAndWith" 
              (isLeft (parser (parseAndWith (,) (parseChar 'a') (parseChar 'a')) ("ab", 0))))
    ]

testParseMany :: Test
testParseMany = TestList [
    TestCase (assertEqual "for (parseMany (parseChar 'a') on \"aaa\")" 
              (Right ("aaa", ("", 3))) 
              (parser (parseMany (parseChar 'a')) ("aaa", 0))),
    TestCase (assertEqual "for (parseMany (parseChar 'a') on empty string)" 
              (Right ("", ("", 0))) 
              (parser (parseMany (parseChar 'a')) ("", 0)))
    ]


testParseFunction :: Test
testParseFunction =
    TestList
        [ TestCase (assertEqual "parseFunction basic" (Right ("", [Function "foo" [] [Return (Number 1)]])) (parseFunction "foo(){return 1;}" []))
        , TestCase (assertEqual "parseFunction nested" (Right ("", [Function "foo" [] [Function "bar" [] [Call "=" [String "bool", Identifier "b", Boolean True]], Return (Number 1)]])) (parseFunction "foo(){ fn bar() {bool b=true;} return 1;}" []))
        , TestCase (assertEqual "parseFunction with params" (Right ("", [Function "foo" ["bar"] [Return (Number 1)]])) (parseFunction "foo(bar){return 1;}" []))
        , TestCase (assertEqual "parseFunction with params and body" (Right ("", [Function "foo" ["bar", "other"] [Return (Number 1)]])) (parseFunction "foo(bar, other ){return 1;}" []))
        , TestCase (assertEqual "parseFunction with params, body and spaces" (Right ("", [Function "foo" ["bar"] [Return (Number 1)]])) (parseFunction "foo ( bar ) { return 1; }" []))
        , TestCase (assertEqual "parseFunction with params, body, spaces and newlines" (Right ("", [Function "foo" ["bar"] [Return (Number 1)]])) (parseFunction "foo ( bar ) {\nreturn 1;\n}" []))
        , TestCase (assertEqual "parseFunction with multi-content" (Right ("", [Function "foo" ["bar"] [Call "=" [String "int", Identifier "a", Number 42], Call "=" [String "int", Identifier "b", Identifier "bar"], Return (Identifier "bar")]])) (parseFunction "foo ( bar ) {\nint a = 42; int b = bar; return bar;\n}" []))
        , TestCase (assertEqual "parseFunction wrong - no parenthesis" (Left "No parenthesis found for function params") (parseFunction "foo{return bar; }" []))
        , TestCase (assertEqual "parseFunction wrong - only open parenthesis" (Left "No parenthesis found for function params") (parseFunction "foo({return bar; }" []))
        , TestCase (assertEqual "parseFunction wrong - only close parenthesis" (Left "No parenthesis found for function params") (parseFunction "foo){return bar; }" []))
        , TestCase (assertEqual "parseFunction wrong - invalid function name" (Left "Variable name 'int' is banned") (parseFunction "int(){return bar; }" []))
        , TestCase (assertEqual "parseFunction wrong - invalid function scope" (Left "Unrecognized element") (parseFunction "foo(){unknown content; }" []))
        ]

testParseOperatorValue :: Test
testParseOperatorValue =
    TestList
        [ TestCase (assertEqual "parseOperatorValue +" (Right (Call "+" [Number 1, Number 1])) (parseOperatorValue " 1 + 1"))
        , TestCase (assertEqual "parseOperatorValue -" (Right (Call "-" [Identifier "i", Number 1])) (parseOperatorValue "i - 1"))
        , TestCase (assertEqual "parseOperatorValue *" (Right (Call "*" [Identifier "i", Number 1])) (parseOperatorValue "i * 1"))
        , TestCase (assertEqual "parseOperatorValue /" (Right (Call "/" [Identifier "i", Number 1])) (parseOperatorValue "i / 1"))
        , TestCase (assertEqual "parseOperatorValue %" (Right (Call "%" [Identifier "i", Number 1])) (parseOperatorValue "i % 1"))
        , TestCase (assertEqual "parseOperatorValue <" (Right (Call "<" [Identifier "i", Number 1])) (parseOperatorValue "i < 1"))
        , TestCase (assertEqual "parseOperatorValue >" (Right (Call ">" [Identifier "i", Number 1])) (parseOperatorValue "i > 1"))
        , TestCase (assertEqual "parseOperatorValue <=" (Right (Call "<=" [Identifier "i", Number 1])) (parseOperatorValue "i <= 1"))
        , TestCase (assertEqual "parseOperatorValue >=" (Right (Call ">=" [Identifier "i", Number 1])) (parseOperatorValue "i >= 1"))
        , TestCase (assertEqual "parseOperatorValue ==" (Right (Call "==" [Identifier "i", Number 1])) (parseOperatorValue "i == 1"))
        , TestCase (assertEqual "parseOperatorValue !=" (Right (Call "!=" [Identifier "i", Number 1])) (parseOperatorValue "i != 1"))
        , TestCase (assertEqual "parseOperatorValue wrong no operator" (Left "Invalid operator expression") (parseOperatorValue "1 1"))
        ]

testParseConditionExpression :: Test
testParseConditionExpression =
    TestList
        [ TestCase (assertEqual "parseConditionExpression basic" (Right (Call "<" [Number 1, Number 2])) (parseConditionExpression "1 < 2"))
        , TestCase (assertEqual "parseConditionExpression with spaces" (Right (Call ">" [Number 1, Number 2])) (parseConditionExpression "1 > 2"))
        , TestCase (assertEqual "parseConditionExpression with spaces" (Right (Call ">=" [Number 1, Number 2])) (parseConditionExpression "1 >= 2"))
        , TestCase (assertEqual "parseConditionExpression with spaces" (Right (Call "<=" [Number 1, Number 2])) (parseConditionExpression "1 <= 2"))
        , TestCase (assertEqual "parseConditionExpression with spaces" (Right (Call "==" [Number 1, Number 2])) (parseConditionExpression "1 == 2"))
        , TestCase (assertEqual "parseConditionExpression with spaces" (Right (Call "!=" [Number 1, Number 2])) (parseConditionExpression "1 != 2"))
        , TestCase (assertEqual "parseConditionExpression with spaces" (Left "Invalid condition expression") (parseConditionExpression " 1 2 "))
        , TestCase (assertEqual "parseConditionExpression with spaces" (Right (Number 1)) (parseConditionExpression "1 "))
        , TestCase (assertEqual "parseConditionExpression with spaces" (Left "Invalid condition expression") (parseConditionExpression " "))
        , TestCase (assertEqual "parseConditionExpression with spaces" (Left "Invalid condition expression") (parseConditionExpression "1 a 1"))
        ]

testParseFunctionCall :: Test
testParseFunctionCall =
    TestList
        [ TestCase (assertEqual "parseFunctionCall basic" (Right (Call "foo" [])) (parseFunctionCall "foo()"))
        , TestCase (assertEqual "parseFunctionCall with param String" (Right (Call "foo" [String "bar"])) (parseFunctionCall "foo(\"bar\")"))
        , TestCase (assertEqual "parseFunctionCall with params Number" (Right (Call "foo" [Number 1])) (parseFunctionCall "foo ( 1 )"))
        , TestCase (assertEqual "parseFunctionCall with params Boolean" (Right (Call "foo" [Boolean True])) (parseFunctionCall "foo ( true )"))
        , TestCase (assertEqual "parseFunctionCall with multi-params" (Right (Call "foo" [Number 1, Number 2])) (parseFunctionCall "foo ( 1 , 2 )"))
        ]

testParseCondition :: Test
testParseCondition =
    TestList
        [ TestCase (assertEqual "parseCondition basic" (Right ("", [If (Boolean True) [Return (Number 1)] []])) (parseCondition " (true) {return 1;}" []))
        , TestCase (assertEqual "parseCondition wrong without scope" (Left "Scope must start with open bracket but empty code found") (parseCondition " (true)" []))
        , TestCase (assertEqual "parseCondition wrong without scope but other char" (Left "Code must start with an open bracket but starts with 'a'") (parseCondition " (true) anything" []))
        , TestCase (assertEqual "parseCondition wrong if without condition" (Left "Missing condition in if statement") (parseCondition "( ) {return 1;}" []))
        , TestCase (assertEqual "parseCondition wrong if with broken close condition parenthesis" (Left "If condition must start with '(' and end with ')'") (parseCondition " ) {return 1;}" []))
        , TestCase (assertEqual "parseCondition wrong if with broken open condition parenthesis" (Left "If condition must start with '(' and end with ')'") (parseCondition " ( {return 1;}" []))
        , TestCase (assertEqual "parseCondition with else" (Right ("", [If (Boolean True) [Return (Number 1)] [Return (Number 2)]])) (parseCondition " (true) {return 1;} else {return 2;}" []))
        , TestCase (assertEqual "parseCondition with elif" (Right ("", [If (Boolean True) [Return (Number 1)] [If (Boolean True) [Return (Number 2)] []]])) (parseCondition "(true) {return 1;} elif (true) {return 2;}" []))
        , TestCase (assertEqual "parseCondition wrong elif without condition" (Left "If condition must start with '(' and end with ')'") (parseCondition " (true) {return 1;} elif {return 2;}" []))
        , TestCase (assertEqual "parseCondition with elif and else" (Right ("", [If (Boolean True) [Call "=" [String "str", Identifier "a", String "test"], Call "=" [Identifier "a", String "other"]] [If (Boolean True) [Return (Number 2)] [Return (Number 3)]]])) (parseCondition "(true) {str a = \"test\"; a = \"other\";} elif (true) {return 2;} else {return 3;}" []))
        , TestCase (assertEqual "parseCondition with elif, else and spaces" (Right ("", [If (Boolean True) [Return (Number 1)] [If (Boolean True) [Call "=" [String "bool", Identifier "c", Boolean False]] [Return (Number 3)]]])) (parseCondition " (true) {return 1;} elif (true) {bool c =  false;} else {return 3;}" []))
        , TestCase (assertEqual "parseCondition with elif, else, spaces and newlines" (Right ("", [If (Boolean True) [Return (Number 1)] [If (Boolean True) [Return (Number 2)] [Return (Number 3)]]])) (parseCondition "(true) {\nreturn 1;\n} elif (true) {\nreturn 2;\n} else {\nreturn 3;\n}" []))
        ]

testParseLoop :: Test
testParseLoop =
    TestList
        [ TestCase (assertEqual "parseLoop while basic" (Right ("", [Loop (Boolean True) [Return (Number 1)] Nothing Nothing])) (parseLoop "while" " (true) {return 1;}" []))
        , TestCase (assertEqual "parseLoop while wrong with broken close condition parenthesis" (Left "Loop condition must start with '(' and end with ')'") (parseLoop "while" ") {return 1;}" []))
        , TestCase (assertEqual "parseLoop while wrong with broken open condition parenthesis" (Left "Loop condition must start with '(' and end with ')'") (parseLoop "while" "( {return 1;}" []))
        , TestCase (assertEqual "parseLoop while with spaces" (Right ("", [Loop (Boolean True) [Return (Number 1)] Nothing Nothing])) (parseLoop "while" "( true ) { return 1; }" []))
        , TestCase (assertEqual "parseLoop while with spaces and newlines" (Right ("", [Loop (Number 1) [Return (Number 1)] Nothing Nothing])) (parseLoop "while" "( 1 ) {\nreturn 1;\n}" []))
        , TestCase (assertEqual "parseLoop while with superior condition" (Right ("", [Loop (Call ">" [Identifier "a", Identifier "b"]) [Return (Number 1)] Nothing Nothing])) (parseLoop "while" "( a > b ) {\nreturn 1;\n}" []))
        ]

testParseDreamberd :: Test
testParseDreamberd =
    TestList
        [ TestCase (assertEqual "parseDreamberd basic assign var" (Right [Call "=" [Identifier "int", Identifier "a", Number 1]]) (parseDreamberd "int a = 1;"))
        , TestCase (assertEqual "parseDreamberd reasign var and call function" (Right [Call "=" [Identifier "int", Identifier "a", Number 1], Call "=" [Identifier "a", Number 2], Call "fct" []]) (parseDreamberd "int a = 1;a = 2;fct();"))
        , TestCase (assertEqual "parseDreamberd wrong assign var value" (Left "Expected ';' but found 'a' at 4") (parseDreamberd "int a 1; "))
        , TestCase (assertEqual "parseDreamberd boolean value" (Right [Call "=" [Identifier "bool", Identifier "a", Number 42]]) (parseDreamberd "bool a=42; "))
        , TestCase (assertEqual "parseDreamberd empty code" (Left "Expected '-' but found end of file at 5") (parseDreamberd "     "))
        , TestCase (assertEqual "parseDreamberd invalid variable name" (Left "Expected ';' but found '*' at 4") (parseDreamberd "int * = 4;" ))
        , TestCase (assertEqual "parseDreamberd basic loop" (Right [Loop (Boolean True) [Call "=" [Identifier "int", Identifier "b", Number 42]] Nothing Nothing]) (parseDreamberd "while (true) {int b = 42;}" ))
        , TestCase (assertEqual "parseDreamberd wrong var type" (Left "Unrecognized element") (parseDreamberd "fluid variable = 89;"))
        , TestCase (assertEqual "parseDreamberd strange function call with parenthesis" (Right [Call "a" [String "lo()l)", Call "b" [Call "c" [String "()"]]]]) (parseDreamberd "a(\"lo()l)\", b(c(\"()\")));" ))
        , TestCase (assertEqual "parseDreamberd define var, += and return it" (Right [Call "=" [String "int", Identifier "i", Number 0], Call "+=" [Identifier "i", Number 1], Return (Identifier "i")]) (parseDreamberd "int i=0;i+=1;return i;" ))
        ]
