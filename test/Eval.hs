module Eval
    ( testEvaluation
    ) where

import Ast (evalAst)
import Test.HUnit (Test(..), assertEqual)
import Types (AstNode (..))

testEvaluation :: Test
testEvaluation = TestList [
    testAstEvaluation
    ]

testAstEvaluation :: Test
testAstEvaluation = TestList [
    TestCase (assertEqual "basic" (evalAst [Types.Number 1]) (Just [Types.Number 1])),
    TestCase (assertEqual "basic operation" (evalAst [Types.Call (Types.Symbol "*") [Types.Number 12, Types.Number 3]]) (Just [Types.Number 36])),
    TestCase (assertEqual "unknown symbol" (evalAst [Types.Call (Types.Symbol "*") [Types.Symbol "var", Types.Number 3]]) Nothing),
    TestCase (assertEqual "unknown call" (evalAst [Types.Call (Types.Symbol "_") [Types.Number 1, Types.Number 3]]) Nothing),
    TestCase (assertEqual "defined symbol" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "a", Types.Number 10], Types.Call (Types.Symbol "+") [Types.Symbol "a", Types.Number 3]]) (Just [Types.Number 13])),
    TestCase (assertEqual "two defined symbols" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "a", Types.Number 10], Types.Call (Types.Symbol "define") [Types.Symbol "b", Types.Number 20], Types.Call (Types.Symbol "-") [Types.Symbol "a", Types.Symbol "b"]]) (Just [Types.Number (-10)])),
    TestCase (assertEqual "combined operations" (evalAst [Types.Call (Types.Symbol "*") [Types.Call (Types.Symbol "/") [Types.Number 10, Types.Number 5], Types.Call (Types.Symbol "%") [Types.Number 19, Types.Number 10]]]) (Just [Types.Number 18])),
    TestCase (assertEqual "operation in symbol" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "test", Types.Call (Types.Symbol "/") [Types.Number 10, Types.Number 5]], Types.Symbol("test")]) (Just [Types.Number 2])),
    TestCase (assertEqual "define without operation" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "const", Types.Number 10]]) (Just [])),
    TestCase (assertEqual "void" (evalAst [Types.Void]) (Just [])),
    TestCase (assertEqual "void with define" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "a", Types.Number 10], Types.Void]) (Just [])),
    TestCase (assertEqual "basic lambda" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "add", Types.Lambda ["a", "b"] (Types.Call (Types.Symbol "+") [Types.Symbol "a", Types.Symbol "b"])], Types.Call (Types.Symbol "add") [Types.Number 12, Types.Number 3]]) (Just [Types.Number 15])),
    TestCase (assertEqual "wrong lambda call" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "add", Types.Lambda ["a", "b"] (Types.Call (Types.Symbol "+") [Types.Symbol "a", Types.Symbol "b"])], Types.Call (Types.Symbol "add") [Types.Number 12, Types.Number 3, Types.Number 1]]) Nothing),
    TestCase (assertEqual "lambda execution" (evalAst [Types.Call (Types.Call (Types.Symbol "lambda") [Types.Lambda ["a", "b"] (Types.Call (Types.Symbol "+") [Types.Symbol "a", Types.Symbol "b"])]) [Types.Number 12, Types.Number 3]]) Nothing)

    -- TODO: I'm bad
    -- ,
    -- TestCase (assertEqual "basic lambda definition" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "add", (Types.Call (Types.Symbol "lambda") [Types.Lambda ["a", "b"] (Types.Call (Types.Symbol "+") [Types.Symbol "a", Types.Symbol "b"])])]]) (Just [])),

    -- TODO: Reassignment not working
    -- ,
    -- TestCase (assertEqual "reassignment" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "let", Types.Number 4], Types.Call (Types.Symbol "define") [Types.Symbol "let", Types.Number 9], Types.Symbol("let")]) (Just [Types.Number 9])),
    -- TestCase (assertEqual "reassignment with value" (evalAst [Types.Call (Types.Symbol "define") [Types.Symbol "let", Types.Number 4], Types.Call (Types.Symbol "define") [Types.Symbol "let", Types.Call (Types.Symbol "+") [Types.Symbol "let", Types.Number 6]], Types.Symbol("let")]) (Just [Types.Number 10]))
    ]
