module TestEval (
    testEvaluation,
) where

import Ast (evalAst)
import Test.HUnit (Test (..), assertEqual)
import Types (AstNode (..), Variable (..))

testEvaluation :: Test
testEvaluation =
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
        [ TestCase (assertEqual "basic" (evalAst [Types.Number 1]) (Just [Types.Number 1]))
        , TestCase (assertEqual "basic operation" (evalAst [Types.Call (Types.Symbol "*") [Types.Number 12, Types.Number 3]]) (Just [Types.Number 36]))
        , TestCase (assertEqual "unknown symbol" (evalAst [Types.Call (Types.Symbol "*") [Types.Symbol "var", Types.Number 3]]) Nothing)
        , TestCase (assertEqual "unknown call" (evalAst [Types.Call (Types.Symbol "_") [Types.Number 1, Types.Number 3]]) Nothing)
        , TestCase (assertEqual "wrong parameters" (evalAst [Types.Call (Types.Symbol "*") [Types.Call (Types.Symbol "+") [Types.Boolean True, Types.Number 1], Types.Number 3]]) Nothing)
        , TestCase (assertEqual "void" (evalAst [Types.Void]) (Just []))
        ]

testDeclarations :: Test
testDeclarations =
    TestList
        [ TestCase (assertEqual "defined symbol" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "a", Types.Number 10], Types.Call (Types.Symbol "+") [Types.Symbol "a", Types.Number 3]]) (Just [Types.Number 13]))
        , TestCase (assertEqual "reversed symbol" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "a", Types.Number 10], Types.Call (Types.Symbol "+") [Types.Number 3, Types.Symbol "a"]]) (Just [Types.Number 13]))
        , TestCase (assertEqual "wrong define" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "a", Types.Number 10, Types.Number 11], Types.Call (Types.Symbol "+") [Types.Number 3, Types.Symbol "a"]]) Nothing)
        , TestCase (assertEqual "two defined symbols" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "a", Types.Number 10], Types.Call (Types.Symbol "define") [Types.Symbol "b", Types.Number 20], Types.Call (Types.Symbol "-") [Types.Symbol "a", Types.Symbol "b"]]) (Just [Types.Number (-10)]))
        , TestCase (assertEqual "combined operations" (evalAst [Types.Call (Types.Symbol "*") [Types.Call (Types.Symbol "/") [Types.Number 10, Types.Number 5], Types.Call (Types.Symbol "%") [Types.Number 19, Types.Number 10]]]) (Just [Types.Number 18]))
        , TestCase (assertEqual "operation in symbol" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "test", Types.Call (Types.Symbol "/") [Types.Number 10, Types.Number 5]], Types.Symbol "test"]) (Just [Types.Number 2]))
        , TestCase (assertEqual "define without operation" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "const", Types.Number 10]]) (Just []))
        , TestCase (assertEqual "void with define" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "a", Types.Number 10], Types.Void]) (Just []))
        ]

testConditions :: Test
testConditions =
    TestList
      [ TestCase (assertEqual "basic if" (evalAst [Types.Call (Types.Symbol "if") [Types.Boolean True, Types.Number 10, Types.Number 12]]) (Just [Types.Number 10]))
      , TestCase (assertEqual "basic if false" (evalAst [Types.Call (Types.Symbol "if") [Types.Boolean False, Types.Number 13, Types.Number 15]]) (Just [Types.Number 15]))
      , TestCase (assertEqual "wrong if" (evalAst [Types.Call (Types.Symbol "if") [Types.Boolean False, Types.Number 13, Types.Number 15, Types.Number 17]]) Nothing)
      , TestCase (assertEqual "if with non bool argument" (evalAst [Types.Call (Types.Symbol "if") [Types.Number 1, Types.Number 13, Types.Number 15]]) (Just [Types.Number 13]))
      , TestCase (assertEqual "if with defines" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "b", Types.Number 7], Types.Call (Types.Symbol "define") [Types.Symbol "a", Types.Number 6], Types.Call (Types.Symbol "if") [Types.Boolean True, Types.Symbol "a", Types.Symbol "b"]]) (Just [Types.Number 6]))
      , TestCase (assertEqual "if with define in condition" (evalAst [Types.Call (Types.Symbol "if") [Types.Call (Types.Symbol "define") [Types.Symbol "cond", Types.Number 10], Types.Symbol "cond", Types.Symbol "aaaaa"]]) (Just [Types.Number 10]))
      , TestCase (assertEqual "if with define in condition but not defined" (evalAst [Types.Call (Types.Symbol "if") [Types.Call (Types.Symbol "define") [Types.Symbol "cond", Types.Number 10], Types.Symbol "aaaaa", Types.Symbol "cond"]]) Nothing)
      , TestCase (assertEqual "if with define in trueCondition" (evalAst [Types.Call (Types.Symbol "if") [Types.Boolean True, Types.Call (Types.Symbol "define") [Types.Symbol "a", Types.Number 10], Types.Number 3], Types.Symbol "a"]) (Just [Types.Number 10]))
      , TestCase (assertEqual "if with define in falseCondition" (evalAst [Types.Call (Types.Symbol "if") [Types.Boolean False, Types.Number 3, Types.Call (Types.Symbol "define") [Types.Symbol "a", Types.Number 15]], Types.Symbol "a"]) (Just [Types.Number 15]))
      ]

testLambdas :: Test
testLambdas =
    TestList
        [ TestCase (assertEqual "basic lambda" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "add", Types.Lambda ["a", "b"] (Types.Call (Types.Symbol "+") [Types.Symbol "a", Types.Symbol "b"])], Types.Call (Types.Symbol "add") [Types.Number 12, Types.Number 3]]) (Just [Types.Number 15]))
        , TestCase (assertEqual "basic function definition" (evalAst [Types.Call (Symbol "define") [Types.Call (Types.Symbol "sum") [Types.Symbol "a", Types.Symbol "b"], Types.Call (Types.Symbol "+") [Types.Symbol "a", Types.Symbol "b"]], Types.Call (Types.Symbol "sum") [Types.Number (-1), Types.Number (-2)]]) (Just [Types.Number (-3)]))
        , TestCase (assertEqual "wrong lambda call" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "add", Types.Lambda ["a", "b"] (Types.Call (Types.Symbol "+") [Types.Symbol "a", Types.Symbol "b"])], Types.Call (Types.Symbol "add") [Types.Number 12, Types.Number 3, Types.Number 1]]) Nothing)
        , TestCase (assertEqual "lambda execution" (evalAst [Types.Call (Types.Call (Types.Symbol "lambda") [Types.Lambda ["a", "b"] (Types.Call (Types.Symbol "+") [Types.Symbol "a", Types.Symbol "b"])]) [Types.Number 12, Types.Number 3]]) Nothing)
        , TestCase (assertEqual "lambda call" (evalAst [Types.Call (Types.Call (Types.Symbol "lambda") [Types.Call (Types.Symbol "x") [], Types.Call (Types.Symbol "*") [Types.Symbol "x", Types.Symbol "x"]]) [Types.Number 5]]) (Just [Types.Number 25]))
        , TestCase (assertEqual "lambda definition" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "sum", Types.Call (Types.Symbol "lambda") [Types.Call (Types.Symbol "a") [Types.Symbol "b"], Types.Call (Types.Symbol "+") [Types.Symbol "a", Types.Symbol "b"]]], Types.Call (Types.Symbol "sum") [Types.Number 1, Types.Number 2]]) (Just [Types.Number 3]))
        ]

testReassignments :: Test
testReassignments =
    TestList
        [ TestCase (assertEqual "reassignment" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "let", Types.Number 4], Types.Call (Types.Symbol "define") [Types.Symbol "let", Types.Number 9], Types.Symbol "let"]) (Just [Types.Number 9]))
        , TestCase (assertEqual "reassignment with value" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "let", Types.Number 4], Types.Call (Types.Symbol "define") [Types.Symbol "let", Types.Call (Types.Symbol "+") [Types.Symbol "let", Types.Number 6]], Types.Symbol "let"]) (Just [Types.Number 10]))
        , TestCase
            ( assertEqual
                "multiple reassignments"
                ( evalAst
                    [ Types.Call (Types.Symbol "define") [Types.Symbol "a", Types.Number 1]
                    , Types.Call (Types.Symbol "define") [Types.Symbol "b", Types.Number 15]
                    , Types.Call (Types.Symbol "define") [Types.Symbol "a", Types.Symbol "b"]
                    , Types.Symbol "a"
                    ]
                )
                (Just [Types.Number 15])
            )
        , TestCase
            ( assertEqual
                "post multiple reassignments"
                ( evalAst
                    [ Types.Call (Types.Symbol "define") [Types.Symbol "a", Types.Number 1]
                    , Types.Call (Types.Symbol "define") [Types.Symbol "b", Types.Number 15]
                    , Types.Call (Types.Symbol "define") [Types.Symbol "a", Types.Symbol "b"]
                    , Types.Call (Types.Symbol "define") [Types.Symbol "b", Types.Number 17]
                    , Types.Symbol "a"
                    , Types.Symbol "b"
                    ]
                )
                (Just [Types.Number 15, Types.Number 17])
            )
        ]

testMisc :: Test
testMisc =
    TestList
        [ TestCase (assertEqual "show variables" (show (Variable "a" (Types.Number 1))) "Variable {identifier = \"a\", value = 1}")
        , TestCase (assertEqual "show ast number" (show (Types.Number 1)) "1")
        , TestCase (assertEqual "show ast call" (show (Types.Call (Types.Symbol "+") [Types.Number 1, Types.Number 2])) "#<procedure>")
        , TestCase (assertEqual "show ast lambda" (show (Types.Lambda ["a", "b"] (Types.Call (Types.Symbol "+") [Types.Symbol "a", Types.Symbol "b"]))) "#<procedure>")
        , TestCase (assertEqual "show ast true" (show (Types.Boolean True)) "#t")
        , TestCase (assertEqual "show ast false" (show (Types.Boolean False)) "#f")
        , TestCase (assertEqual "show ast symbol" (show (Types.Symbol "False")) "False")
        ]
