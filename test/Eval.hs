module Eval
    ( testEvaluation
    ) where

import Ast (AstNode (..), evalAst)
import Test.HUnit (Test(..), assertEqual)

testEvaluation :: Test
testEvaluation = TestList [
    testAstEvaluation
    ]

testAstEvaluation :: Test
testAstEvaluation = TestList [
    TestCase (assertEqual "basic" (evalAst [Ast.Number 1]) (Just [Ast.Number 1])),
    TestCase (assertEqual "basic operation" (evalAst [Ast.Call "*" [Ast.Number 12, Ast.Number 3]]) (Just [Ast.Number 36])),
    TestCase (assertEqual "unknown symbol" (evalAst [Ast.Call "*" [Ast.Symbol "var", Ast.Number 3]]) Nothing),
    TestCase (assertEqual "defined symbol" (evalAst [Ast.Call "define" [Ast.Symbol "a", Ast.Number 10], Ast.Call "+" [Ast.Symbol "a", Ast.Number 3]]) (Just [Ast.Number 13])),
    TestCase (assertEqual "two defined symbols" (evalAst [Ast.Call "define" [Ast.Symbol "a", Ast.Number 10], Ast.Call "define" [Ast.Symbol "b", Ast.Number 20], Ast.Call "-" [Ast.Symbol "a", Ast.Symbol "b"]]) (Just [Ast.Number (-10)])),
    TestCase (assertEqual "combined operations" (evalAst [Ast.Call "*" [Ast.Call "/" [Ast.Number 10, Ast.Number 5], Ast.Call "%" [Ast.Number 19, Ast.Number 10]]]) (Just [Ast.Number 18])),
    TestCase (assertEqual "operation in symbol" (evalAst [Ast.Call "define" [Ast.Symbol "test", Ast.Call "/" [Ast.Number 10, Ast.Number 5]], Ast.Symbol("test")]) (Just [Ast.Number 2])),
    TestCase (assertEqual "define without operation" (evalAst [Ast.Call "define" [Ast.Symbol "const", Ast.Number 10]]) (Just []))
    --TestCase (assertEqual "reassignment" (evalAst [Ast.Call "define" [Ast.Symbol "let", Ast.Number 4], Ast.Call "define" [Ast.Symbol "let", Ast.Number 9], Ast.Symbol("let")]) (Just [Ast.Number 9])),
    --TestCase (assertEqual "reassignment with value" (evalAst [Ast.Call "define" [Ast.Symbol "let", Ast.Number 4], Ast.Call "define" [Ast.Symbol "let", Ast.Call "+" [Ast.Symbol "let", Ast.Number 6]], Ast.Symbol("let")]) (Just [Ast.Number 10]))
    ]
