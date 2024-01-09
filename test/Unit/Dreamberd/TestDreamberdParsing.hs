module Unit.Dreamberd.TestDreamberdParsing (testDreamberdParsing) where

import Dreamberd.Parsing.Elements.Condition (parseConditionExpression)
import Dreamberd.Parsing.Elements.Operator (parseExpression)
import Dreamberd.Parsing.Main (parseCondition, parseDreamberd, parseFunction)
import Dreamberd.Parsing.Values (parseFunctionCall)
import Dreamberd.Types (AstNode (AssignVariable, Boolean, Call, Function, Identifier, If, Number, Operator, Return, String))
import Test.HUnit (Test (..), assertEqual)

testDreamberdParsing :: Test
testDreamberdParsing = TestList [testParseFunction, testParseFunctionCall, testParseCondition, testparseExpression, testParseConditionExpression, testParseDreamberd]

testParseFunction :: Test
testParseFunction =
    TestList
        [ TestCase (assertEqual "parseFunction basic" (Right ("", [Function "foo" [] [Return (Number 1)]])) (parseFunction "foo(){return 1;}" []))
        , TestCase (assertEqual "parseFunction nested" (Right ("", [Function "foo" [] [Function "bar" [] [AssignVariable "bool" "b" (Boolean True)], Return (Number 1)]])) (parseFunction "foo(){ function bar() {bool b = true;} return 1;}" []))
        , TestCase (assertEqual "parseFunction with params" (Right ("", [Function "foo" ["bar"] [Return (Number 1)]])) (parseFunction "foo(bar){return 1;}" []))
        , TestCase (assertEqual "parseFunction with params and body" (Right ("", [Function "foo" ["bar", "other"] [Return (Number 1)]])) (parseFunction "foo(bar, other ){return 1;}" []))
        , TestCase (assertEqual "parseFunction with params, body and spaces" (Right ("", [Function "foo" ["bar"] [Return (Number 1)]])) (parseFunction "foo ( bar ) { return 1; }" []))
        , TestCase (assertEqual "parseFunction with params, body, spaces and newlines" (Right ("", [Function "foo" ["bar"] [Return (Number 1)]])) (parseFunction "foo ( bar ) {\nreturn 1;\n}" []))
        , TestCase (assertEqual "parseFunction with multi-content" (Right ("", [Function "foo" ["bar"] [AssignVariable "int" "a" (Number 42), AssignVariable "int" "b" (Identifier "bar"), Return (Identifier "bar")]])) (parseFunction "foo ( bar ) {\nint a = 42; int b = bar; return bar;\n}" []))
        , TestCase (assertEqual "parseFunction wrong - no parenthesis" (Left "No parenthesis found for function params") (parseFunction "foo{return bar; }" []))
        , TestCase (assertEqual "parseFunction wrong - only open parenthesis" (Left "No parenthesis found for function params") (parseFunction "foo({return bar; }" []))
        , TestCase (assertEqual "parseFunction wrong - only close parenthesis" (Left "No parenthesis found for function params") (parseFunction "foo){return bar; }" []))
        , TestCase (assertEqual "parseFunction wrong - invalid function name" (Left "Variable name 'int' is banned") (parseFunction "int(){return bar; }" []))
        , TestCase (assertEqual "parseFunction wrong - invalid function scope" (Left "Unrecognized element") (parseFunction "foo(){unknown content; }" []))
        ]

testparseExpression :: Test
testparseExpression =
    TestList
        [ TestCase (assertEqual "parseExpression =" (Right (Operator "=" (Number 1) (Number 1))) (parseExpression " 1 = 1"))
        , TestCase (assertEqual "parseExpression wrong" (Left "Invalid expression") (parseExpression "1 1"))
        , TestCase (assertEqual "parseExpression +=" (Right (Operator "+=" (Identifier "i") (Number 1))) (parseExpression "i += 1"))
        , TestCase (assertEqual "parseExpression -= without spaces" (Right (Operator "-=" (Identifier "i") (Number 1))) (parseExpression "i -= 1"))
        , TestCase (assertEqual "parseExpression *=" (Right (Operator "*=" (Identifier "i") (Number 1))) (parseExpression "i *= 1"))
        , TestCase (assertEqual "parseExpression /=" (Right (Operator "/=" (Identifier "i") (Number 1))) (parseExpression "i /= 1"))
        , TestCase (assertEqual "parseExpression %=" (Right (Operator "%=" (Identifier "i") (Number 1))) (parseExpression "i %= 1"))
        , TestCase (assertEqual "parseExpression +=" (Right (Operator "+=" (Identifier "i") (Number 1))) (parseExpression "i += 1"))
        , TestCase (assertEqual "parseExpression =+" (Left "Invalid operator") (parseExpression "i =+ 1"))
        ]

testParseConditionExpression :: Test
testParseConditionExpression =
    TestList
        [ TestCase (assertEqual "parseConditionExpression basic" (Right (Operator "<" (Number 1) (Number 2))) (parseConditionExpression "1 < 2"))
        , TestCase (assertEqual "parseConditionExpression with spaces" (Right (Operator ">" (Number 1) (Number 2))) (parseConditionExpression "1 > 2"))
        , TestCase (assertEqual "parseConditionExpression with spaces" (Right (Operator ">=" (Number 1) (Number 2))) (parseConditionExpression "1 >= 2"))
        , TestCase (assertEqual "parseConditionExpression with spaces" (Right (Operator "<=" (Number 1) (Number 2))) (parseConditionExpression "1 <= 2"))
        , TestCase (assertEqual "parseConditionExpression with spaces" (Right (Operator "==" (Number 1) (Number 2))) (parseConditionExpression "1 == 2"))
        , TestCase (assertEqual "parseConditionExpression with spaces" (Right (Operator "!=" (Number 1) (Number 2))) (parseConditionExpression "1 != 2"))
        , TestCase (assertEqual "parseConditionExpression with spaces" (Left "Invalid expression") (parseConditionExpression " 1 2 "))
        , TestCase (assertEqual "parseConditionExpression with spaces" (Right (Number 1)) (parseConditionExpression "1 "))
        , TestCase (assertEqual "parseConditionExpression with spaces" (Left "Invalid expression") (parseConditionExpression " "))
        , TestCase (assertEqual "parseConditionExpression with spaces" (Left "Invalid expression") (parseConditionExpression "1 a 1"))
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
        [ TestCase (assertEqual "parseCondition basic" (Right ("", [If (Boolean True) [Return (Number 1)] []])) (parseCondition " (true) {return 1;}" []))
        , TestCase (assertEqual "parseCondition wrong if without condition" (Left "Missing condition in if statement") (parseCondition "( ) {return 1;}" []))
        , TestCase (assertEqual "parseCondition wrong if with broken close condition parenthesis" (Left "If condition must start with '(' and end with ')'") (parseCondition " ) {return 1;}" []))
        , TestCase (assertEqual "parseCondition wrong if with broken open condition parenthesis" (Left "If condition must start with '(' and end with ')'") (parseCondition " ( {return 1;}" []))
        , TestCase (assertEqual "parseCondition with else" (Right ("", [If (Boolean True) [Return (Number 1)] [Return (Number 2)]])) (parseCondition " (true) {return 1;} else {return 2;}" []))
        , TestCase (assertEqual "parseCondition with elif" (Right ("", [If (Boolean True) [Return (Number 1)] [If (Boolean True) [Return (Number 2)] []]])) (parseCondition "(true) {return 1;} elif (true) {return 2;}" []))
        , TestCase (assertEqual "parseCondition wrong elif without condition" (Left "If condition must start with '(' and end with ')'") (parseCondition " (true) {return 1;} elif {return 2;}" []))
        , TestCase (assertEqual "parseCondition with elif and else" (Right ("", [If (Boolean True) [AssignVariable "str" "a" (String "test"), Operator "=" (Identifier "a") (String "other")] [If (Boolean True) [Return (Number 2)] [Return (Number 3)]]])) (parseCondition "(true) {str a = \"test\"; a = \"other\";} elif (true) {return 2;} else {return 3;}" []))
        , TestCase (assertEqual "parseCondition with elif, else and spaces" (Right ("", [If (Boolean True) [Return (Number 1)] [If (Boolean True) [Return (Number 2)] [Return (Number 3)]]])) (parseCondition " (true) {return 1;} elif (true) {return 2;} else {return 3;}" []))
        , TestCase (assertEqual "parseCondition with elif, else, spaces and newlines" (Right ("", [If (Boolean True) [Return (Number 1)] [If (Boolean True) [Return (Number 2)] [Return (Number 3)]]])) (parseCondition "(true) {\nreturn 1;\n} elif (true) {\nreturn 2;\n} else {\nreturn 3;\n}" []))
        ]

testParseDreamberd :: Test
testParseDreamberd =
    TestList
        [ TestCase (assertEqual "parseDreamberd basic assign var" (Right [AssignVariable "int" "a" (Number 1)]) (parseDreamberd "int a = 1;" []))
        , TestCase (assertEqual "parseDreamberd reasign var" (Right [AssignVariable "int" "a" (Number 1), Operator "=" (Identifier "a") (Number 2)]) (parseDreamberd "int a = 1;a = 2;" []))
        , TestCase (assertEqual "parseDreamberd wrong assign var value" (Left "Expected '=' after variable name") (parseDreamberd "int a 1; " []))
        , TestCase (assertEqual "parseDreamberd empty code" (Right []) (parseDreamberd "     " []))
        , TestCase (assertEqual "parseDreamberd invalid variable name" (Left "No variable name found") (parseDreamberd "int * = 4;" []))
        ]
