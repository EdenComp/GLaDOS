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
            (Right (Number 123, ("", ("", 1, 4))))
            (parse parseNumber ("123", ("", 1, 1))))
    ]

testParseAndWith :: Test
testParseAndWith = TestList [
    TestCase (assertEqual "for (parseAndWith (,) (parseChar 'a') (parseChar 'a') on \"aa\")"
        (Right (('a', 'a'), ("", ("", 1, 3))))
        (parse (parseAndWith (,) (parseChar 'a') (parseChar 'a')) ("aa", ("", 1, 1)))),
    TestCase (assertBool "for failure case of parseAndWith"
        (case parse (parseAndWith (,) (parseChar 'a') (parseChar 'a')) ("ab", ("", 1, 1)) of
            Left _ -> True
            Right _ -> False))
    ]

testParseMany :: Test
testParseMany =
    TestList [TestCase (assertEqual "for (parseMany (parseChar 'a') on \"aaa\")"
        (Right ("aaa", ("", ("", 1, 4))))
        (parse (parseMany (parseChar 'a')) ("aaa", ("", 1, 1))))
        ,TestCase (assertEqual "for (parseMany (parseChar 'a') on empty string)"  (Right ("", ("", ("", 1, 1)))) (parse (parseMany (parseChar 'a')) ("", ("",1, 1))))
    ]

testParseDreamberd :: Test
testParseDreamberd =
    TestList [
    -- test for var assignement 
    TestCase (assertEqual "parseDreamberd basic assign var"
        (Right [Call "=" [Identifier "int", Identifier "a", Number 1]])
        (parseDreamberd (File "" "int a = 1;"))),
    TestCase (assertEqual "parse variable assignment no space"
        (Right [Call "=" [Identifier "int", Identifier "a", Number 1]])
        (parseDreamberd (File "" "int a=1;"))),
    TestCase (assertEqual "parse variable assignment no type"
        (Right [Call "=" [Identifier "a", Number 1]])
        (parseDreamberd (File "" "a = 1;"))),
    TestCase (assertEqual "parse variable assignment no type no space"
        (Right [Call "=" [Identifier "a", Number 1]])
        (parseDreamberd (File "" "a=1;"))),
    TestCase (assertEqual "parse variable assignment with calculation basic"
        (Right [Call "=" [Identifier "int", Identifier "a", Call "+" [Number 1, Number 2]]])
        (parseDreamberd (File "" "int a = 1 + 2;"))),
    TestCase (assertEqual "parse variable assignment with calculation no space"
        (Right [Call "=" [Identifier "int", Identifier "a", Call "+" [Number 1, Number 2]]])
        (parseDreamberd (File "" "int a=1+2;"))),
    TestCase (assertEqual "parse variable assignment with calculation no type"
        (Right [Call "=" [Identifier "a", Call "+" [Number 1, Number 2]]])
        (parseDreamberd (File "" "a = 1 + 2;"))),
    TestCase (assertEqual "parse variable assignment with calculation complex"
        (Right [Call "=" [Identifier "int", Identifier "a", Call "*" [Number 1, Call "+" [Number 2, Call "/" [Number 3, Number 4]] ]]])
        (parseDreamberd (File "" "int a = 1 * 2 + 3 / 4;"))),
    TestCase (assertEqual "parse variable assignment with function call"
        (Right [Call "=" [Identifier "int", Identifier "a", Call "myFunc" [Number 1]]])
        (parseDreamberd (File "" "int a = myFunc(1);"))),
    TestCase (assertEqual "parse variable assignment with function call no space"
        (Right [Call "=" [Identifier "int", Identifier "a", Call "myFunc" [Number 1]]])
        (parseDreamberd (File "" "int a=myFunc(1);"))),
    TestCase (assertEqual "parse variable assignment with another variable"
        (Right [Call "=" [Identifier "int", Identifier "a", Identifier "b"]])
        (parseDreamberd (File "" "int a = b;"))),
    TestCase (assertEqual "parse variable assignment with another variable no space"
        (Right [Call "=" [Identifier "int", Identifier "a", Identifier "b"]])
        (parseDreamberd (File "" "int a=b;"))),
    TestCase (assertEqual "parse variable assignment with another variable no type"
        (Right [Call "=" [Identifier "int", Identifier "a", Call "if" [Boolean True]]])
        (parseDreamberd (File "" "int a = if(true);"))),
    -- Test for if statement
    TestCase (assertEqual "parse if statement"
        (Right [If (Boolean True) [Number 1] [Number 2]])
        (parseDreamberd (File "" "if (true) {1;} else {2;}"))),
    TestCase (assertEqual "parse if statement no space"
        (Right [If (Boolean True) [Number 1] [Number 2]])
        (parseDreamberd (File "" "if(true){1;}else{2;}"))),
    TestCase (assertEqual "parse if statement no else"
        (Right [If (Boolean True) [Number 1] []])
        (parseDreamberd (File "" "if (true) {1;}"))),
    TestCase (assertEqual "parse if statement no else no space"
        (Right [If (Boolean True) [Number 1] []])
        (parseDreamberd (File "" "if(true){1;}"))),
    TestCase (assertEqual "parse if statement with else if"
        (Right [If (Boolean True) [Number 1] [If (Boolean False) [Number 2] []]])
        (parseDreamberd (File "" "if (true) {1;} elif (false) {2;}"))),
    TestCase (assertEqual "parse if statement with else if and else"
        (Right [If (Boolean True) [Number 1] [If (Boolean False) [Number 2] [Number 3]]])
        (parseDreamberd (File "" "if (true) {1;} elif (false) {2;} else {3;}"))),
    TestCase (assertEqual "parse if statement with calculation"
        (Right [If (Call "==" [Number 1, Number 2]) [Number 1] [Number 2]])
        (parseDreamberd (File "" "if (1 == 2) {1;} else {2;}"))),
    TestCase (assertEqual "parse if statement with calculation no space"
        (Right [If (Call "==" [Number 1, Number 2]) [Number 1] [Number 2]])
        (parseDreamberd (File "" "if(1==2){1;}else{2;}"))),
    TestCase (assertEqual "parse if statement with function call"
        (Right [If (Call "myFunc" [Number 1]) [Number 1] [Number 2]])
        (parseDreamberd (File "" "if (myFunc(1)) {1;} else {2;}"))),
    TestCase (assertEqual "parse if statement calcul and comparison"
        (Right [If (Call "==" [Call "+" [Number 1, Number 2], Number 3]) [Number 1] [Number 2]])
        (parseDreamberd (File "" "if ((1 + 2) == 3) {1;} else {2;}"))),

    -- Test for function declaration
    TestCase (assertEqual "parse function declaration"
        (Right [Function "myFunc" ["x"] [Return (Identifier "x")]])
        (parseDreamberd (File "" "fn myFunc(x) {return x;}"))),
    TestCase (assertEqual "parse function declaration no space"
        (Right [Function "myFunc" ["x"] [Return (Identifier "x")]])
        (parseDreamberd (File "" "fn myFunc(x){return x;}"))),
    TestCase (assertEqual "parse function declaration no args"
        (Right [Function "myFunc" [] [Return (Identifier "x")]])
        (parseDreamberd (File "" "fn myFunc() {return x;}"))),
    TestCase (assertEqual "parse function declaration no args no parenthesis"
        (Right [Function "myFunc" [] [Return (Identifier "x")]])
        (parseDreamberd (File "" "fn myFunc {return x;}"))),
    TestCase (assertEqual "parse function declaration with call to another function"
        (Right [Function "myFunc" [] [Call "myFunc2" []]])
        (parseDreamberd (File "" "fn myFunc {myFunc2();}"))),
    TestCase (assertEqual "parse function declaration with call to another function no space"
        (Right [Function "myFunc" [] [Call "myFunc2" []]])
        (parseDreamberd (File "" "fn myFunc{myFunc2();}"))),
    TestCase (assertEqual "parse function declaration with call to another function with args"
        (Right [Function "myFunc" [] [Call "myFunc2" [Number 1]]])
        (parseDreamberd (File "" "fn myFunc {myFunc2(1);}"))),
    TestCase (assertEqual "parse function declaration with if else"
        (Right [Function "myFunc" [] [If (Boolean True) [Number 1] [Number 2]]])
        (parseDreamberd (File "" "fn myFunc {if (true) {1;} else {2;}}"))),
    TestCase (assertEqual "parse function declaration with if else and return value"
        (Right [Function "myFunc" [] [If (Boolean True) [Return (Number 1)] [Return (Number 2)]]])
        (parseDreamberd (File "" "fn myFunc {if (true) {return 1;} else {return 2;}}"))),


    -- Test for function call
    TestCase (assertEqual "parse function call"
        (Right [Call "myFunc" [Number 3]])
        (parseDreamberd (File "" "myFunc(3);"))),
    TestCase (assertEqual "parse function call no space"
        (Right [Call "myFunc" [Number 3]])
        (parseDreamberd (File "" "myFunc(3);"))),
    TestCase (assertEqual "parse function call no args"
        (Right [Call "myFunc" []])
        (parseDreamberd (File "" "myFunc();"))),
    TestCase (assertEqual "parse function call no args no parenthesis"
        (Right [Identifier "myFunc"])
        (parseDreamberd (File "" "myFunc;"))),
    TestCase (assertEqual "parse function call with multiple args"
        (Right [Call "myFunc" [Number 1, Number 2, Number 3]])
        (parseDreamberd (File "" "myFunc(1, 2, 3);"))),
    TestCase (assertEqual "parse function call with multiple args no space"
        (Right [Call "myFunc" [Number 1, Number 2, Number 3]])
        (parseDreamberd (File "" "myFunc(1,2,3);"))),
    TestCase (assertEqual "parse function call with multiple args no parenthesis"
        (Left ":1:8: Expected ';' but found '1'")
        (parseDreamberd (File "" "myFunc 1, 2, 3;"))),

    -- Test for loop statement
    TestCase (assertEqual "parse while loop"
        (Right [Loop (Boolean True) [Number 1] Nothing Nothing])
        (parseDreamberd (File "" "while (true) {1;}"))),
    TestCase (assertEqual "parse while loop no space"
        (Right [Loop (Boolean True) [Number 1] Nothing Nothing])
        (parseDreamberd (File "" "while(true){1;}"))),

    -- Test for binary operation
    TestCase (assertEqual "parse binary operation"
        (Right [Call "+" [Number 2, Number 3]])
        (parseDreamberd (File "" "2 + 3;"))),

    -- Test for multiple statements
    TestCase (assertEqual "parse multiple statements"
        (Right [Call "=" [Identifier "int", Identifier "a", Number 1], Call "+" [Identifier "a", Number 2]])
        (parseDreamberd (File "" "int a = 1; a + 2;")))
    ]


