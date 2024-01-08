module Unit.Dreamberd.TestDreamberdParsing (testDreamberdParsing) where

import Dreamberd.Parsing.Main (parseCondition, parseFunction)
import Dreamberd.Parsing.Values (parseFunctionCall)
import Dreamberd.Types (AstNode (Boolean, Call, Function, Identifier, If, Number, Operator, Return, String))
import Test.HUnit (Test (..), assertEqual)
import Dreamberd.Parsing.Elements.Operator (parseOperator)
import Dreamberd.Parsing.Elements.Condition (parseConditionExpression)

testDreamberdParsing :: Test
testDreamberdParsing = TestList [testParseFunction, testParseFunctionCall, testParseCondition, testParseOperator, testParseConditionExpression]

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

testParseOperator :: Test
testParseOperator =
    TestList
        [ TestCase (assertEqual "parseOperator <" (Right ("<", "1")) (parseOperator "< 1"))
        , TestCase (assertEqual "parseOperator <=" (Right ("<=", "1")) (parseOperator "<= 1"))
        , TestCase (assertEqual "parseOperator >" (Right (">", "1")) (parseOperator "> 1"))
        , TestCase (assertEqual "parseOperator >=" (Right (">=", "1")) (parseOperator ">= 1"))
        , TestCase (assertEqual "parseOperator ==" (Right ("==", "1")) (parseOperator "== 1"))
        , TestCase (assertEqual "parseOperator !=" (Right ("!=", "1")) (parseOperator "!= 1"))
        , TestCase (assertEqual "parseOperator =" (Right ("=", "1")) (parseOperator "= 1"))
        , TestCase (assertEqual "parseOperator with spaces" (Left "No operator found") (parseOperator " a 1"))
        , TestCase (assertEqual "parseOperator with spaces" (Left "No operator found") (parseOperator " 1"))
        ]

testParseConditionExpression :: Test
testParseConditionExpression =
    TestList
        [TestCase (assertEqual "parseConditionExpression basic" (Right (Operator "<" (Number 1) (Number 2))) (parseConditionExpression "1 < 2"))
        ,TestCase (assertEqual "parseConditionExpression with spaces" (Right (Operator ">" (Number 1) (Number 2))) (parseConditionExpression "1 > 2"))
        ,TestCase (assertEqual "parseConditionExpression with spaces" (Right (Operator ">=" (Number 1) (Number 2))) (parseConditionExpression "1 >= 2"))
        ,TestCase (assertEqual "parseConditionExpression with spaces" (Right (Operator "<=" (Number 1) (Number 2))) (parseConditionExpression "1 <= 2"))
        ,TestCase (assertEqual "parseConditionExpression with spaces" (Right (Operator "==" (Number 1) (Number 2))) (parseConditionExpression "1 == 2"))
        ,TestCase (assertEqual "parseConditionExpression with spaces" (Right (Operator "!=" (Number 1) (Number 2))) (parseConditionExpression "1 != 2"))
        ,TestCase (assertEqual "parseConditionExpression with spaces" (Left "No operator found") (parseConditionExpression " 1 2 "))
        ,TestCase (assertEqual "parseConditionExpression with spaces" (Left "No operator found") (parseConditionExpression "1 "))
        ,TestCase (assertEqual "parseConditionExpression with spaces" (Left "No value found") (parseConditionExpression " "))
        ,TestCase (assertEqual "parseConditionExpression with spaces" (Left "No operator found") (parseConditionExpression "1 a 1"))
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


