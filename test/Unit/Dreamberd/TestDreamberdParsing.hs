module Unit.Dreamberd.TestDreamberdParsing (testDreamberdParsing) where

import Dreamberd.Parser (parse, parseAndWith, parseBinaryOperator, parseChar, parseDreamberd, parseInteger, parseMany)
import Dreamberd.Types (AstNode (..), File (File))
import Test.HUnit (Test (..), assertBool, assertEqual)

testDreamberdParsing :: Test
testDreamberdParsing = TestList [testParseInteger, testParseDreamberd, testParseAndWith, testParseMany, testParseBinaryOperator]

testParseInteger :: Test
testParseInteger =
    TestList
        [ TestCase
            ( assertEqual
                "for (parseInteger \"123\")"
                (Right (123, ("", ("", 1, 4))))
                (parse parseInteger ("123", ("", 1, 1)))
            )
        ]

testParseAndWith :: Test
testParseAndWith =
    TestList
        [ TestCase
            ( assertEqual
                "for (parseAndWith (,) (parseChar 'a') (parseChar 'a') on \"aa\")"
                (Right (('a', 'a'), ("", ("", 1, 3))))
                (parse (parseAndWith (,) (parseChar 'a') (parseChar 'a')) ("aa", ("", 1, 1)))
            )
        , TestCase
            ( assertBool
                "for failure case of parseAndWith"
                ( case parse (parseAndWith (,) (parseChar 'a') (parseChar 'a')) ("ab", ("", 1, 1)) of
                    Left _ -> True
                    Right _ -> False
                )
            )
        ]

testParseMany :: Test
testParseMany =
    TestList
        [ TestCase
            ( assertEqual
                "for (parseMany (parseChar 'a') on \"aaa\")"
                (Right ("aaa", ("", ("", 1, 4))))
                (parse (parseMany (parseChar 'a')) ("aaa", ("", 1, 1)))
            )
        , TestCase (assertEqual "for (parseMany (parseChar 'a') on empty string)" (Right ("", ("", ("", 1, 1)))) (parse (parseMany (parseChar 'a')) ("", ("", 1, 1))))
        ]

testParseBinaryOperator :: Test
testParseBinaryOperator =
    TestList
        [ TestCase
            ( assertEqual
                "parse addition operator"
                (Right ("+", ("", ("", 1, 2))))
                (parse parseBinaryOperator ("+", ("", 1, 1)))
            )
        , TestCase
            ( assertEqual
                "parse subtraction operator"
                (Right ("-", ("", ("", 1, 2))))
                (parse parseBinaryOperator ("-", ("", 1, 1)))
            )
        , TestCase
            ( assertEqual
                "parse multiplication operator"
                (Right ("*", ("", ("", 1, 2))))
                (parse parseBinaryOperator ("*", ("", 1, 1)))
            )
        , TestCase
            ( assertEqual
                "parse division operator"
                (Right ("/", ("", ("", 1, 2))))
                (parse parseBinaryOperator ("/", ("", 1, 1)))
            )
        , TestCase
            ( assertEqual
                "parse modulus operator"
                (Right ("%", ("", ("", 1, 2))))
                (parse parseBinaryOperator ("%", ("", 1, 1)))
            )
        , TestCase
            ( assertEqual
                "parse equality operator"
                (Right ("==", ("", ("", 1, 3))))
                (parse parseBinaryOperator ("==", ("", 1, 1)))
            )
        , TestCase
            ( assertEqual
                "parse inequality operator"
                (Right ("!=", ("", ("", 1, 3))))
                (parse parseBinaryOperator ("!=", ("", 1, 1)))
            )
        , TestCase
            ( assertEqual
                "parse less than operator"
                (Right ("<", ("", ("", 1, 2))))
                (parse parseBinaryOperator ("<", ("", 1, 1)))
            )
        , TestCase
            ( assertEqual
                "parse less than or equal operator"
                (Right ("<=", ("", ("", 1, 3))))
                (parse parseBinaryOperator ("<=", ("", 1, 1)))
            )
        , TestCase
            ( assertEqual
                "parse greater than operator"
                (Right (">", ("", ("", 1, 2))))
                (parse parseBinaryOperator (">", ("", 1, 1)))
            )
        , TestCase
            ( assertEqual
                "parse greater than or equal operator"
                (Right (">=", ("", ("", 1, 3))))
                (parse parseBinaryOperator (">=", ("", 1, 1)))
            )
        , TestCase
            ( assertEqual
                "parse and operator"
                (Right ("&&", ("", ("", 1, 3))))
                (parse parseBinaryOperator ("&&", ("", 1, 1)))
            )
        , TestCase
            ( assertEqual
                "parse or operator"
                (Right ("||", ("", ("", 1, 3))))
                (parse parseBinaryOperator ("||", ("", 1, 1)))
            )
        , TestCase
            ( assertEqual
                "parse xor operator"
                (Right ("^", ("", ("", 1, 2))))
                (parse parseBinaryOperator ("^", ("", 1, 1)))
            )
        , TestCase
            ( assertEqual
                "parse bitwise and operator"
                (Left ":1:1: Expected '%' but found '&'")
                (parse parseBinaryOperator ("&", ("", 1, 1)))
            )
        , TestCase
            ( assertEqual
                "parse bitwise or operator"
                (Left ":1:1: Expected '%' but found '|'")
                (parse parseBinaryOperator ("|", ("", 1, 1)))
            )
        ]

testParseDreamberd :: Test
testParseDreamberd =
    TestList
        [ -- test for var assignement
          TestCase
            ( assertEqual
                "parseDreamberd basic assign var"
                (Right [Call (Identifier "=") [Identifier "int", Identifier "a", Integer 1]])
                (parseDreamberd (File "" "int a = 1;"))
            )
        , TestCase
            ( assertEqual
                "parse variable assignment no space"
                (Right [Call (Identifier "=") [Identifier "int", Identifier "a", Integer 1]])
                (parseDreamberd (File "" "int a=1;"))
            )
        , TestCase
            ( assertEqual
                "parse variable assignment no type"
                (Right [Call (Identifier "=") [Identifier "a", Integer 1]])
                (parseDreamberd (File "" "a = 1;"))
            )
        , TestCase
            ( assertEqual
                "parse variable assignment no type no space"
                (Right [Call (Identifier "=") [Identifier "a", Integer 1]])
                (parseDreamberd (File "" "a=1;"))
            )
        , TestCase
            ( assertEqual
                "parse variable assignment with calculation basic"
                (Right [Call (Identifier "=") [Identifier "int", Identifier "a", Call (Identifier "+")  [Integer 1, Integer 2]]])
                (parseDreamberd (File "" "int a = 1 + 2;"))
            )
        , TestCase
            ( assertEqual
                "parse variable assignment with calculation no space"
                (Right [Call (Identifier "=") [Identifier "int", Identifier "a", Call (Identifier "+")  [Integer 1, Integer 2]]])
                (parseDreamberd (File "" "int a=1+2;"))
            )
        , TestCase
            ( assertEqual
                "parse variable assignment with calculation no type"
                (Right [Call (Identifier "=") [Identifier "a", Call (Identifier "+")  [Integer 1, Integer 2]]])
                (parseDreamberd (File "" "a = 1 + 2;"))
            )
        , TestCase
            ( assertEqual
                "parse variable assignment with calculation complex"
                (Right [Call (Identifier "=") [Identifier "int", Identifier "a", Call (Identifier "*") [Integer 1, Call (Identifier "+")  [Integer 2, Call (Identifier "/") [Integer 3, Integer 4]]]]])
                (parseDreamberd (File "" "int a = 1 * 2 + 3 / 4;"))
            )
        , TestCase
            ( assertEqual
                "parse variable assignment with function call"
                (Right [Call (Identifier "=") [Identifier "int", Identifier "a", Call (Identifier "myFunc") [Integer 1]]])
                (parseDreamberd (File "" "int a = myFunc(1);"))
            )
        , TestCase
            ( assertEqual
                "parse variable assignment with function call no space"
                (Right [Call (Identifier "=") [Identifier "int", Identifier "a", Call (Identifier "myFunc") [Integer 1]]])
                (parseDreamberd (File "" "int a=myFunc(1);"))
            )
        , TestCase
            ( assertEqual
                "parse variable assignment with another variable"
                (Right [Call (Identifier "=") [Identifier "int", Identifier "a", Identifier "b"]])
                (parseDreamberd (File "" "int a = b;"))
            )
        , TestCase
            ( assertEqual
                "parse variable assignment with another variable no space"
                (Right [Call (Identifier "=") [Identifier "int", Identifier "a", Identifier "b"]])
                (parseDreamberd (File "" "int a=b;"))
            )
        , TestCase
            ( assertEqual
                "parse variable assignment with another variable no type"
                (Right [Call (Identifier "=") [Identifier "int", Identifier "a", Call (Identifier "if") [Boolean True]]])
                (parseDreamberd (File "" "int a = if(true);"))
            )
        , -- Test for if statement
          TestCase
            ( assertEqual
                "parse if statement"
                (Right [If (Boolean True) [Integer 1] [Integer 2]])
                (parseDreamberd (File "" "if (true) {1;} else {2;}"))
            )
        , TestCase
            ( assertEqual
                "parse if statement no space"
                (Right [If (Boolean True) [Integer 1] [Integer 2]])
                (parseDreamberd (File "" "if(true){1;}else{2;}"))
            )
        , TestCase
            ( assertEqual
                "parse if statement no else"
                (Right [If (Boolean True) [Integer 1] []])
                (parseDreamberd (File "" "if (true) {1;}"))
            )
        , TestCase
            ( assertEqual
                "parse if statement no else no space"
                (Right [If (Boolean True) [Integer 1] []])
                (parseDreamberd (File "" "if(true){1;}"))
            )
        , TestCase
            ( assertEqual
                "parse if statement with else if"
                (Right [If (Boolean True) [Integer 1] [If (Boolean False) [Integer 2] []]])
                (parseDreamberd (File "" "if (true) {1;} elif (false) {2;}"))
            )
        , TestCase
            ( assertEqual
                "parse if statement with else if and else"
                (Right [If (Boolean True) [Integer 1] [If (Boolean False) [Integer 2] [Integer 3]]])
                (parseDreamberd (File "" "if (true) {1;} elif (false) {2;} else {3;}"))
            )
        , TestCase
            ( assertEqual
                "parse if statement with calculation"
                (Right [If (Call (Identifier "==") [Integer 1, Integer 2]) [Integer 1] [Integer 2]])
                (parseDreamberd (File "" "if (1 == 2) {1;} else {2;}"))
            )
        , TestCase
            ( assertEqual
                "parse if statement with calculation no space"
                (Right [If (Call (Identifier "==") [Integer 1, Integer 2]) [Integer 1] [Integer 2]])
                (parseDreamberd (File "" "if(1==2){1;}else{2;}"))
            )
        , TestCase
            ( assertEqual
                "parse if statement with function call"
                (Right [If (Call (Identifier "myFunc") [Integer 1]) [Integer 1] [Integer 2]])
                (parseDreamberd (File "" "if (myFunc(1)) {1;} else {2;}"))
            )
        , TestCase
            ( assertEqual
                "parse if statement calcul and comparison"
                (Right [If (Call (Identifier "==") [Call (Identifier "+")  [Integer 1, Integer 2], Integer 3]) [Integer 1] [Integer 2]])
                (parseDreamberd (File "" "if ((1 + 2) == 3) {1;} else {2;}"))
            )
        , -- Test for function declaration
          TestCase
            ( assertEqual
                "parse function declaration"
                (Right [Function "myFunc" ["x"] [Return $ Just $ Identifier "x"]])
                (parseDreamberd (File "" "fn myFunc(x) {return x;}"))
            )
        , TestCase
            ( assertEqual
                "parse function declaration no space"
                (Right [Function "myFunc" ["x"] [Return $ Just $ Identifier "x"]])
                (parseDreamberd (File "" "fn myFunc(x){return x;}"))
            )
        , TestCase
            ( assertEqual
                "parse function declaration no args"
                (Right [Function "myFunc" [] [Return $ Just $ Identifier "x"]])
                (parseDreamberd (File "" "fn myFunc() {return x;}"))
            )
        , TestCase
            ( assertEqual
                "parse function declaration no args no parenthesis"
                (Right [Function "myFunc" [] [Return $ Just $ Identifier "x"]])
                (parseDreamberd (File "" "fn myFunc {return x;}"))
            )
        , TestCase
            ( assertEqual
                "parse function declaration with call to another function"
                (Right [Function "myFunc" [] [Call (Identifier "myFunc2") []]])
                (parseDreamberd (File "" "fn myFunc {myFunc2();}"))
            )
        , TestCase
            ( assertEqual
                "parse function declaration with call to another function no space"
                (Right [Function "myFunc" [] [Call (Identifier "myFunc2") []]])
                (parseDreamberd (File "" "fn myFunc{myFunc2();}"))
            )
        , TestCase
            ( assertEqual
                "parse function declaration with call to another function with args"
                (Right [Function "myFunc" [] [Call (Identifier "myFunc2") [Integer 1]]])
                (parseDreamberd (File "" "fn myFunc {myFunc2(1);}"))
            )
        , TestCase
            ( assertEqual
                "parse function declaration with if else"
                (Right [Function "myFunc" [] [If (Boolean True) [Integer 1] [Integer 2]]])
                (parseDreamberd (File "" "fn myFunc {if (true) {1;} else {2;}}"))
            )
        , TestCase
            ( assertEqual
                "parse function declaration with if else and return value"
                (Right [Function "myFunc" [] [If (Boolean True) [Return $ Just $ Integer 1] [Return $ Just $ Integer 2]]])
                (parseDreamberd (File "" "fn myFunc {if (true) {return 1;} else {return 2;}}"))
            )
        , -- Test for function call
          TestCase
            ( assertEqual
                "parse function call"
                (Right [Call (Identifier "myFunc") [Integer 3]])
                (parseDreamberd (File "" "myFunc(3);"))
            )
        , TestCase
            ( assertEqual
                "parse function call no space"
                (Right [Call (Identifier "myFunc") [Integer 3]])
                (parseDreamberd (File "" "myFunc(3);"))
            )
        , TestCase
            ( assertEqual
                "parse function call no args"
                (Right [Call (Identifier "myFunc") []])
                (parseDreamberd (File "" "myFunc();"))
            )
        , TestCase
            ( assertEqual
                "parse function call no args no parenthesis"
                (Right [Identifier "myFunc"])
                (parseDreamberd (File "" "myFunc;"))
            )
        , TestCase
            ( assertEqual
                "parse function call with multiple args"
                (Right [Call (Identifier "myFunc") [Integer 1, Integer 2, Integer 3]])
                (parseDreamberd (File "" "myFunc(1, 2, 3);"))
            )
        , TestCase
            ( assertEqual
                "parse function call with multiple args no space"
                (Right [Call (Identifier "myFunc") [Integer 1, Integer 2, Integer 3]])
                (parseDreamberd (File "" "myFunc(1,2,3);"))
            )
        , TestCase
            ( assertEqual
                "parse function call with multiple args no parenthesis"
                (Left ":1:8: Expected ';' but found '1'")
                (parseDreamberd (File "" "myFunc 1, 2, 3;"))
            )
        , -- Test for loop statement
          TestCase
            ( assertEqual
                "parse while loop"
                (Right [Loop (Boolean True) [Integer 1] Nothing Nothing])
                (parseDreamberd (File "" "while (true) {1;}"))
            )
        , TestCase
            ( assertEqual
                "parse while loop no space"
                (Right [Loop (Boolean True) [Integer 1] Nothing Nothing])
                (parseDreamberd (File "" "while(true){1;}"))
            )
        , TestCase
            ( assertEqual
                "parse while loop no parenthesis"
                (Left ":1:7: Expected ';' but found 't'")
                (parseDreamberd (File "" "while true {1;}"))
            )
        , TestCase
            ( assertEqual
                "parse while loop with function call"
                (Right [Loop (Call (Identifier "myFunc") []) [Integer 1] Nothing Nothing])
                (parseDreamberd (File "" "while (myFunc()) {1;}"))
            )
        , TestCase
            ( assertEqual
                "parse while loop with calculation"
                (Right [Loop (Call (Identifier "==") [Integer 1, Integer 2]) [Integer 1] Nothing Nothing])
                (parseDreamberd (File "" "while (1 == 2) {1;}"))
            )
        , TestCase
            ( assertEqual
                "parse while loop with calculation no space"
                (Right [Loop (Call (Identifier "==") [Integer 1, Integer 2]) [Integer 1] Nothing Nothing])
                (parseDreamberd (File "" "while(1==2){1;}"))
            )
        , TestCase
            ( assertEqual
                "parse while loop with calculation complex"
                (Right [Loop (Call (Identifier "==") [Call (Identifier "+")  [Integer 1, Integer 2], Integer 3]) [Integer 1] Nothing Nothing])
                (parseDreamberd (File "" "while ((1 + 2) == 3) {1;}"))
            )
        , -- Test for binary operation
          TestCase
            ( assertEqual
                "parse binary operation"
                (Right [Call (Identifier "+")  [Integer 2, Integer 3]])
                (parseDreamberd (File "" "2 + 3;"))
            )
        , TestCase
            ( assertEqual
                "parse binary operation no space"
                (Right [Call (Identifier "+")  [Integer 2, Integer 3]])
                (parseDreamberd (File "" "2+3;"))
            )
        , TestCase
            ( assertEqual
                "parse binary operation with parenthesis"
                (Right [Call (Identifier "+")  [Integer 2, Call (Identifier "*") [Integer 3, Integer 4]]])
                (parseDreamberd (File "" "2 + (3 * 4);"))
            )
        , TestCase
            ( assertEqual
                "parse binary operation with parenthesis no space"
                (Right [Call (Identifier "+")  [Integer 2, Call (Identifier "*") [Integer 3, Integer 4]]])
                (parseDreamberd (File "" "2+(3*4);"))
            )
        , TestCase
            ( assertEqual
                "parse binary operation with parenthesis and function call"
                (Right [Call (Identifier "+")  [Integer 2, Call (Identifier "*") [Call (Identifier "myFunc") [Integer 3], Integer 4]]])
                (parseDreamberd (File "" "2 + (myFunc(3) *            \n\n 4);"))
            )
        , TestCase
            ( assertEqual
                "parse binary operation with parenthesis and function call no space"
                (Right [Call (Identifier "+")  [Integer 2, Call (Identifier "*") [Call (Identifier "myFunc") [Integer 3], Integer 4]]])
                (parseDreamberd (File "" "2+(myFunc(3)*4)\n;"))
            )
        , TestCase
            ( assertEqual
                "parse binary operation with parenthesis and mutliple function call"
                (Right [Call (Identifier "+")  [Integer 2, Call (Identifier "*") [Call (Identifier "myFunc") [Integer 3], Call (Identifier "myFunc2") [Integer 4]]]])
                (parseDreamberd (File "" "2 + (myFunc(3) *                 myFunc2(                             \n4));"))
            )
        , TestCase
            ( assertEqual
                "parse binary operation with parenthesis and a lot of function call"
                (Right [Call (Identifier "+")  [Integer 2, Call (Identifier "*") [Call (Identifier "myFunc") [Integer 3], Call (Identifier "myFunc2") [Call (Identifier "myFunc3") [Integer 4]]]]])
                (parseDreamberd (File "" "2 + (myFunc(3) * myFunc2(myFunc3(4)));"))
            )
        , -- Test for multiple statements
          TestCase
            ( assertEqual
                "parse multiple statements"
                (Right [Call (Identifier "=") [Identifier "int", Identifier "a", Integer 1], Call (Identifier "+")  [Identifier "a", Integer 2]])
                (parseDreamberd (File "" "int a = 1; a \n\n\n\n+ 2;"))
            )
        , TestCase
            ( assertEqual
                "parse multiple statements no space"
                (Right [Call (Identifier "=") [Identifier "int", Identifier "a", Integer 1], Call (Identifier "+")  [Identifier "a", Integer 2]])
                (parseDreamberd (File "" "int a=1;a+2;"))
            )
        , TestCase
            ( assertEqual
                "parse multiple statements with function call"
                (Right [Call (Identifier "=") [Identifier "int", Identifier "a", Integer 1], Call (Identifier "+")  [Identifier "a", Call (Identifier "myFunc") [Integer 2]]])
                (parseDreamberd (File "" "int a = 1; a + myFunc(2);"))
            )
        , TestCase
            ( assertEqual
                "parse multiple statements with function call no space"
                (Right [Call (Identifier "=") [Identifier "int", Identifier "a", Integer 1], Call (Identifier "+")  [Identifier "a", Call (Identifier "myFunc") [Integer 2]]])
                (parseDreamberd (File "" "int a=1;a+myFunc(2)\n\n\n;"))
            )
        , TestCase
            ( assertEqual
                "parse a strlen function in dreamberd4"
                (Right [Function "strlen" ["str"] [If (Call (Identifier "==") [Identifier "str", String ""]) [Return $ Just $ Integer 0] [Return $ Just $ Call (Identifier "+")  [Integer 1, Call (Identifier "strlen") [Call (Identifier "substr") [Identifier "str", Integer 1]]]]]])
                (parseDreamberd (File "" "fn strlen(str) {if (str \n== \"\") {return \n\n0\n\n;} \n\nelse {\n\nreturn \n1 + strlen(substr(\nstr, 1));}}"))
            )
        , TestCase
            ( assertEqual
                "parse a strlen function in dreamberd4 no space"
                (Right [Function "strlen" ["str"] [If (Call "==" [Identifier "str", String ""]) [Return $ Just $ Integer 0] [Return $ Just $ Call "+" [Integer 1, Call "strlen" [Call "substr" [Identifier "str", Integer 1]]]]]])
                (parseDreamberd (File "" "fn strlen(str){if(str==\"\"){return 0;}else{return 1+strlen(substr(str,1));}}"))
            )
        , TestCase
            ( assertEqual
                "parse a strcmp function in dreamberd4"
                (Right [Function "strcmp" ["str1", "str2"] [If (Call "==" [Identifier "str1", String ""]) [If (Call "==" [Identifier "str2", String ""]) [Return $ Just $ Integer 0] [Return $ Just $ Call "-" [Integer 1]]] [If (Call "==" [Identifier "str2", String ""]) [Return $ Just $ Integer 1] [If (Call "==" [Call "substr" [Identifier "str1", Integer 0, Integer 1], Call "substr" [Identifier "str2", Integer 0, Integer 1]]) [Return $ Just $ Call "strcmp" [Call "substr" [Identifier "str1", Integer 1], Call "substr" [Identifier "str2", Integer 1]]] [If (Call "<" [Call "substr" [Identifier "str1", Integer 0, Integer 1], Call "substr" [Identifier "str2", Integer 0, Integer 1]]) [Return $ Just $ Call "-" [Integer 1]] [Return $ Just $ Integer 1]]]]]])
                (parseDreamberd (File "" "fn strcmp(str1, str2) {\n\nif (str1 == \"\") {\n\nif (str2 == \"\") {\n\nreturn 0;\n\n} else {\n\nreturn -1;\n\n}\n\n} else {\n\nif (str2 == \"\") {\n\nreturn 1;\n\n} else {\n\nif (substr(str1, 0, 1) == substr(str2, 0, 1)) {\n\nreturn strcmp(substr(str1, 1), substr(str2, 1));\n\n} else {\n\nif (substr(str1, 0, 1) < substr(str2, 0, 1)) {\n\nreturn -1;\n\n} else {\n\nreturn 1;\n\n}\n\n}\n\n}\n\n}\n\n}"))
            )
        , TestCase
            ( assertEqual
                "parse a strcmp function in dreamberd4 no space"
                (Right [Function "strcmp" ["str1", "str2"] [If (Call "==" [Identifier "str1", String ""]) [If (Call "==" [Identifier "str2", String ""]) [Return $ Just $ Integer 0] [Return $ Just $ Call "-" [Integer 1]]] [If (Call "==" [Identifier "str2", String ""]) [Return $ Just $ Integer 1] [If (Call "==" [Call "substr" [Identifier "str1", Integer 0, Integer 1], Call "substr" [Identifier "str2", Integer 0, Integer 1]]) [Return $ Just $ Call "strcmp" [Call "substr" [Identifier "str1", Integer 1], Call "substr" [Identifier "str2", Integer 1]]] [If (Call "<" [Call "substr" [Identifier "str1", Integer 0, Integer 1], Call "substr" [Identifier "str2", Integer 0, Integer 1]]) [Return $ Just $ Call "-" [Integer 1]] [Return $ Just $ Integer 1]]]]]])
                (parseDreamberd (File "" "fn strcmp(str1,str2){if(str1==\"\"){if(str2==\"\"){return 0;}else{return -1;}}else{if(str2==\"\"){return 1;}else{if(substr(str1,0,1)==substr(str2,0,1)){return strcmp(substr(str1,1),substr(str2,1));}else{if(substr(str1,0,1)<substr(str2,0,1)){return -1;}else{return 1;}}}}}\n"))
            )
        , -- Test that should fail
          TestCase
            ( assertEqual
                "parse a strcmp function in dreamberd4 no space"
                (Left ":1:3: Expected ';' but found 's'")
                (parseDreamberd (File "" "a strcmp(str1,str2){if(str1==\"\"){if(str2==\"\"){return 0;}else{return -1;}}else{if(str2==\"\"){return 1;}else{if(substr(str1,0,1)==substr(str2,0,1)){return strcmp(substr(str1,1),substr(str2,1));}else{if(substr(str1,0,1)<substr(str2,0,1)){return -1;}else{return 1;}}}}}\n"))
            )
        , TestCase
            ( assertEqual
                "nothing coherent"
                (Left ":1:11: Expected ';' but found 'j'")
                (parseDreamberd (File "" "kezenfjzn j ifna , ok,d"))
            )
        , TestCase
            ( assertEqual
                "empty file"
                (Right [])
                (parseDreamberd (File "" ""))
            )
        , TestCase
            ( assertEqual
                "empty file with space"
                (Left "' ' at :1:1")
                (parseDreamberd (File "" " "))
            )
        , TestCase
            ( assertEqual
                "empty file with newline"
                (Left "'\n' at :1:1")
                (parseDreamberd (File "" "\n"))
            )
        , TestCase
            ( assertEqual
                "empty file with tab"
                (Left "'\t' at :1:1")
                (parseDreamberd (File "" "\t"))
            )
        , TestCase
            ( assertEqual
                "empty file with carriage return"
                (Left "'\r' at :1:1")
                (parseDreamberd (File "" "\r"))
            )
        , TestCase
            ( assertEqual
                "empty file with carriage return and newline"
                (Left "'\r\n' at :1:1")
                (parseDreamberd (File "" "\r\n"))
            )
        , TestCase
            ( assertEqual
                "file with one function definition but no ;"
                (Left ":1:3: Expected ';' but found 'f'")
                (parseDreamberd (File "" "a fn() {return 1;}"))
            )
        , TestCase
            ( assertEqual
                "file with one function definition but no ; no space"
                (Left ":1:6: Expected ';' but found '{'")
                (parseDreamberd (File "" "afn(){return 1;}"))
            )
        ]
