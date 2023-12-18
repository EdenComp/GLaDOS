module TestSExpr
    ( testSExpr
    ) where

import SExpr (SymbolicExpression(..), sexprToAst, sexprsToAsts)
import Test.HUnit (Test(..), assertEqual)
import Types (AstNode (..))

testSExpr :: Test
testSExpr = TestList [
    testSExprToAST,
    testManySExprToAST
    ]

testSExprToAST :: Test
testSExprToAST = TestList [
    TestCase (assertEqual "basic" (sexprToAst (SExpr.Number 10)) (Just (Types.Number 10))),
    TestCase (assertEqual "boolean true" (sexprToAst (SExpr.Symbol "#t")) (Just (Types.Boolean True))),
    TestCase (assertEqual "boolean false" (sexprToAst (SExpr.Symbol "#f")) (Just (Types.Boolean False))),
    TestCase (assertEqual "list" (sexprToAst (SExpr.List [SExpr.Symbol "if", SExpr.Symbol "#t", SExpr.Number 1, SExpr.Number 2])) (Just (Types.Call (Types.Symbol "if") [Types.Boolean True, Types.Number 1, Types.Number 2]))),
    TestCase (assertEqual "empty" (sexprToAst (SExpr.List [])) (Just (Types.Symbol "()"))),
    TestCase (assertEqual "list with only one element" (sexprToAst (SExpr.List [SExpr.Number 2])) (Just (Types.Call (Types.Number 2) [])))
    ]

testManySExprToAST :: Test
testManySExprToAST = TestList [
    TestCase (assertEqual "basic" (sexprsToAsts [SExpr.Number 11]) (Just [Types.Number 11])),
    TestCase (assertEqual "function with call" (sexprsToAsts [SExpr.List [SExpr.Symbol "define", SExpr.Symbol "number", SExpr.Number 1], SExpr.Symbol "number"]) (Just [Types.Call (Types.Symbol "define") [Types.Symbol "number", Types.Number 1], Types.Symbol "number"]))
    ]
