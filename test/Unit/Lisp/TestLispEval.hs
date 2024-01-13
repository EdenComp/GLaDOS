module Unit.Lisp.TestLispEval (
    testLispEvaluation,
) where

import Lisp.Ast (evalLispAst)
import Lisp.Types (AstNode (..), Variable (..))
import Test.HUnit (Test (..), assertEqual)

testLispEvaluation :: Test
testLispEvaluation =
    TestList
        [ testOperations
        , testLambdas
        , testDeclarations
        , testConditions
        , testReassignments
        , testMisc
        ]

testOperations :: Test
testOperations =
    TestList
        [ TestCase (assertEqual "basic" (evalLispAst [Lisp.Types.Number 1]) (Just [Lisp.Types.Number 1]))
        , TestCase (assertEqual "basic operation" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "*") [Lisp.Types.Number 12, Lisp.Types.Number 3]]) (Just [Lisp.Types.Number 36]))
        , TestCase (assertEqual "unknown symbol" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "*") [Lisp.Types.Symbol "var", Lisp.Types.Number 3]]) Nothing)
        , TestCase (assertEqual "unknown call" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "_") [Lisp.Types.Number 1, Lisp.Types.Number 3]]) Nothing)
        , TestCase (assertEqual "modulo" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "mod") [Lisp.Types.Number 1, Lisp.Types.Number 3]]) (Just [Lisp.Types.Number 1]))
        , TestCase (assertEqual "wrong parameters" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "*") [Lisp.Types.Call (Lisp.Types.Symbol "+") [Lisp.Types.Boolean True, Lisp.Types.Number 1], Lisp.Types.Number 3]]) Nothing)
        , TestCase (assertEqual "void" (evalLispAst [Lisp.Types.Void]) (Just []))
        ]

testDeclarations :: Test
testDeclarations =
    TestList
        [ TestCase (assertEqual "defined symbol" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "a", Lisp.Types.Number 10], Lisp.Types.Call (Lisp.Types.Symbol "+") [Lisp.Types.Symbol "a", Lisp.Types.Number 3]]) (Just [Lisp.Types.Number 13]))
        , TestCase (assertEqual "reversed symbol" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "a", Lisp.Types.Number 10], Lisp.Types.Call (Lisp.Types.Symbol "+") [Lisp.Types.Number 3, Lisp.Types.Symbol "a"]]) (Just [Lisp.Types.Number 13]))
        , TestCase (assertEqual "wrong define" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "a", Lisp.Types.Number 10, Lisp.Types.Number 11], Lisp.Types.Call (Lisp.Types.Symbol "+") [Lisp.Types.Number 3, Lisp.Types.Symbol "a"]]) Nothing)
        , TestCase (assertEqual "two defined symbols" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "a", Lisp.Types.Number 10], Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "b", Lisp.Types.Number 20], Lisp.Types.Call (Lisp.Types.Symbol "-") [Lisp.Types.Symbol "a", Lisp.Types.Symbol "b"]]) (Just [Lisp.Types.Number (-10)]))
        , TestCase (assertEqual "combined operations" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "*") [Lisp.Types.Call (Lisp.Types.Symbol "/") [Lisp.Types.Number 10, Lisp.Types.Number 5], Lisp.Types.Call (Lisp.Types.Symbol "%") [Lisp.Types.Number 19, Lisp.Types.Number 10]]]) (Just [Lisp.Types.Number 18]))
        , TestCase (assertEqual "operation in symbol" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "test", Lisp.Types.Call (Lisp.Types.Symbol "div") [Lisp.Types.Number 10, Lisp.Types.Number 5]], Lisp.Types.Symbol "test"]) (Just [Lisp.Types.Number 2]))
        , TestCase (assertEqual "define without operation" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "const", Lisp.Types.Number 10]]) (Just []))
        , TestCase (assertEqual "void with define" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "a", Lisp.Types.Number 10], Lisp.Types.Void]) (Just []))
        ]

testConditions :: Test
testConditions =
    TestList
        [ TestCase (assertEqual "basic if" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "if") [Lisp.Types.Boolean True, Lisp.Types.Number 10, Lisp.Types.Number 12]]) (Just [Lisp.Types.Number 10]))
        , TestCase (assertEqual "basic if false" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "if") [Lisp.Types.Boolean False, Lisp.Types.Number 13, Lisp.Types.Number 15]]) (Just [Lisp.Types.Number 15]))
        , TestCase (assertEqual "wrong if" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "if") [Lisp.Types.Boolean False, Lisp.Types.Number 13, Lisp.Types.Number 15, Lisp.Types.Number 17]]) Nothing)
        , TestCase (assertEqual "if with non bool argument" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "if") [Lisp.Types.Number 1, Lisp.Types.Number 13, Lisp.Types.Number 15]]) (Just [Lisp.Types.Number 13]))
        , TestCase (assertEqual "if with defines" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "b", Lisp.Types.Number 7], Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "a", Lisp.Types.Number 6], Lisp.Types.Call (Lisp.Types.Symbol "if") [Lisp.Types.Boolean True, Lisp.Types.Symbol "a", Lisp.Types.Symbol "b"]]) (Just [Lisp.Types.Number 6]))
        , TestCase (assertEqual "if with define in condition" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "if") [Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "cond", Lisp.Types.Number 10], Lisp.Types.Symbol "cond", Lisp.Types.Symbol "aaaaa"]]) (Just [Lisp.Types.Number 10]))
        , TestCase (assertEqual "if with define in condition but not defined" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "if") [Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "cond", Lisp.Types.Number 10], Lisp.Types.Symbol "aaaaa", Lisp.Types.Symbol "cond"]]) Nothing)
        , TestCase (assertEqual "if with define in trueCondition" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "if") [Lisp.Types.Boolean True, Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "a", Lisp.Types.Number 10], Lisp.Types.Number 3], Lisp.Types.Symbol "a"]) (Just [Lisp.Types.Number 10]))
        , TestCase (assertEqual "if with define in falseCondition" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "if") [Lisp.Types.Boolean False, Lisp.Types.Number 3, Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "a", Lisp.Types.Number 15]], Lisp.Types.Symbol "a"]) (Just [Lisp.Types.Number 15]))
        , TestCase (assertEqual "greater" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol ">") [Lisp.Types.Number 1, Lisp.Types.Number 2]]) (Just [Lisp.Types.Boolean False]))
        , TestCase (assertEqual "eq with define" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "b", Lisp.Types.Number 7], Lisp.Types.Call (Lisp.Types.Symbol "eq?") [Lisp.Types.Symbol "b", Lisp.Types.Symbol "b"]]) (Just [Lisp.Types.Boolean True]))
        , TestCase (assertEqual "eq with constants" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "=") [Lisp.Types.Number 2, Lisp.Types.Number 3]]) (Just [Lisp.Types.Boolean False]))
        , TestCase (assertEqual "less with define" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "b", Lisp.Types.Number 7], Lisp.Types.Call (Lisp.Types.Symbol "<") [Lisp.Types.Symbol "b", Lisp.Types.Number 10]]) (Just [Lisp.Types.Boolean True]))
        , TestCase (assertEqual "less eq with operation" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "<=") [Lisp.Types.Call (Lisp.Types.Symbol "+") [Lisp.Types.Number 6, Lisp.Types.Number 4], Lisp.Types.Number 10]]) (Just [Lisp.Types.Boolean True]))
        , TestCase (assertEqual "greater eq with operation" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol ">=") [Lisp.Types.Number 10, Lisp.Types.Call (Lisp.Types.Symbol "+") [Lisp.Types.Number 6, Lisp.Types.Number 3]]]) (Just [Lisp.Types.Boolean False]))
        ]

testLambdas :: Test
testLambdas =
    TestList
        [ TestCase (assertEqual "basic lambda" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "add", Lisp.Types.Lambda ["a", "b"] (Lisp.Types.Call (Lisp.Types.Symbol "+") [Lisp.Types.Symbol "a", Lisp.Types.Symbol "b"])], Lisp.Types.Call (Lisp.Types.Symbol "add") [Lisp.Types.Number 12, Lisp.Types.Number 3]]) (Just [Lisp.Types.Number 15]))
        , TestCase (assertEqual "basic function definition" (evalLispAst [Lisp.Types.Call (Symbol "define") [Lisp.Types.Call (Lisp.Types.Symbol "sum") [Lisp.Types.Symbol "a", Lisp.Types.Symbol "b"], Lisp.Types.Call (Lisp.Types.Symbol "+") [Lisp.Types.Symbol "a", Lisp.Types.Symbol "b"]], Lisp.Types.Call (Lisp.Types.Symbol "sum") [Lisp.Types.Number (-1), Lisp.Types.Number (-2)]]) (Just [Lisp.Types.Number (-3)]))
        , TestCase (assertEqual "wrong lambda call" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "add", Lisp.Types.Lambda ["a", "b"] (Lisp.Types.Call (Lisp.Types.Symbol "+") [Lisp.Types.Symbol "a", Lisp.Types.Symbol "b"])], Lisp.Types.Call (Lisp.Types.Symbol "add") [Lisp.Types.Number 12, Lisp.Types.Number 3, Lisp.Types.Number 1]]) Nothing)
        , TestCase (assertEqual "lambda execution" (evalLispAst [Lisp.Types.Call (Lisp.Types.Call (Lisp.Types.Symbol "lambda") [Lisp.Types.Lambda ["a", "b"] (Lisp.Types.Call (Lisp.Types.Symbol "+") [Lisp.Types.Symbol "a", Lisp.Types.Symbol "b"])]) [Lisp.Types.Number 12, Lisp.Types.Number 3]]) Nothing)
        , TestCase (assertEqual "lambda call" (evalLispAst [Lisp.Types.Call (Lisp.Types.Call (Lisp.Types.Symbol "lambda") [Lisp.Types.Call (Lisp.Types.Symbol "x") [], Lisp.Types.Call (Lisp.Types.Symbol "*") [Lisp.Types.Symbol "x", Lisp.Types.Symbol "x"]]) [Lisp.Types.Number 5]]) (Just [Lisp.Types.Number 25]))
        , TestCase (assertEqual "lambda definition" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "sum", Lisp.Types.Call (Lisp.Types.Symbol "lambda") [Lisp.Types.Call (Lisp.Types.Symbol "a") [Lisp.Types.Symbol "b"], Lisp.Types.Call (Lisp.Types.Symbol "+") [Lisp.Types.Symbol "a", Lisp.Types.Symbol "b"]]], Lisp.Types.Call (Lisp.Types.Symbol "sum") [Lisp.Types.Number 1, Lisp.Types.Number 2]]) (Just [Lisp.Types.Number 3]))
        ]

testReassignments :: Test
testReassignments =
    TestList
        [ TestCase (assertEqual "reassignment" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "let", Lisp.Types.Number 4], Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "let", Lisp.Types.Number 9], Lisp.Types.Symbol "let"]) (Just [Lisp.Types.Number 9]))
        , TestCase (assertEqual "reassignment with value" (evalLispAst [Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "let", Lisp.Types.Number 4], Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "let", Lisp.Types.Call (Lisp.Types.Symbol "+") [Lisp.Types.Symbol "let", Lisp.Types.Number 6]], Lisp.Types.Symbol "let"]) (Just [Lisp.Types.Number 10]))
        , TestCase
            ( assertEqual
                "multiple reassignments"
                ( evalLispAst
                    [ Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "a", Lisp.Types.Number 1]
                    , Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "b", Lisp.Types.Number 15]
                    , Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "a", Lisp.Types.Symbol "b"]
                    , Lisp.Types.Symbol "a"
                    ]
                )
                (Just [Lisp.Types.Number 15])
            )
        , TestCase
            ( assertEqual
                "post multiple reassignments"
                ( evalLispAst
                    [ Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "a", Lisp.Types.Number 1]
                    , Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "b", Lisp.Types.Number 15]
                    , Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "a", Lisp.Types.Symbol "b"]
                    , Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "b", Lisp.Types.Number 17]
                    , Lisp.Types.Symbol "a"
                    , Lisp.Types.Symbol "b"
                    ]
                )
                (Just [Lisp.Types.Number 15, Lisp.Types.Number 17])
            )
        ]

testMisc :: Test
testMisc =
    TestList
        [ TestCase (assertEqual "show variables" (show (Variable "a" (Lisp.Types.Number 1))) "Variable {identifier = \"a\", value = 1}")
        , TestCase (assertEqual "show ast number" (show (Lisp.Types.Number 1)) "1")
        , TestCase (assertEqual "show ast call" (show (Lisp.Types.Call (Lisp.Types.Symbol "+") [Lisp.Types.Number 1, Lisp.Types.Number 2])) "#<procedure>")
        , TestCase (assertEqual "show ast lambda" (show (Lisp.Types.Lambda ["a", "b"] (Lisp.Types.Call (Lisp.Types.Symbol "+") [Lisp.Types.Symbol "a", Lisp.Types.Symbol "b"]))) "#<procedure>")
        , TestCase (assertEqual "show ast true" (show (Lisp.Types.Boolean True)) "#t")
        , TestCase (assertEqual "show ast false" (show (Lisp.Types.Boolean False)) "#f")
        , TestCase (assertEqual "show ast symbol" (show (Lisp.Types.Symbol "False")) "False")
        ]
