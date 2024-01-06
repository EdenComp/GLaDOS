module Unit.Lisp.TestLispSExpr (
    testLispSExpr,
) where

import Lisp.SExpr (SymbolicExpression (..), lispSexprsToAsts, sexprToAst)
import Lisp.Types (AstNode (..))
import Test.HUnit (Test (..), assertEqual)

testLispSExpr :: Test
testLispSExpr =
    TestList
        [ testSExprToAST
        , testManySExprToAST
        , testSExprShow
        ]

testSExprToAST :: Test
testSExprToAST =
    TestList
        [ TestCase (assertEqual "basic" (sexprToAst (Lisp.SExpr.Number 10)) (Just (Lisp.Types.Number 10)))
        , TestCase (assertEqual "boolean true" (sexprToAst (Lisp.SExpr.Symbol "#t")) (Just (Lisp.Types.Boolean True)))
        , TestCase (assertEqual "boolean false" (sexprToAst (Lisp.SExpr.Symbol "#f")) (Just (Lisp.Types.Boolean False)))
        , TestCase (assertEqual "list" (sexprToAst (Lisp.SExpr.List [Lisp.SExpr.Symbol "if", Lisp.SExpr.Symbol "true", Lisp.SExpr.Number 1, Lisp.SExpr.Number 2])) (Just (Lisp.Types.Call (Lisp.Types.Symbol "if") [Lisp.Types.Boolean True, Lisp.Types.Number 1, Lisp.Types.Number 2])))
        , TestCase (assertEqual "list" (sexprToAst (Lisp.SExpr.List [Lisp.SExpr.Symbol "if", Lisp.SExpr.Symbol "false", Lisp.SExpr.Symbol "a", Lisp.SExpr.Number 2])) (Just (Lisp.Types.Call (Lisp.Types.Symbol "if") [Lisp.Types.Boolean False, Lisp.Types.Symbol "a", Lisp.Types.Number 2])))
        , TestCase (assertEqual "empty" (sexprToAst (Lisp.SExpr.List [])) (Just (Lisp.Types.Symbol "()")))
        , TestCase (assertEqual "list with one element" (sexprToAst (Lisp.SExpr.List [Lisp.SExpr.Number 2])) (Just (Lisp.Types.Call (Lisp.Types.Number 2) [])))
        ]

testManySExprToAST :: Test
testManySExprToAST =
    TestList
        [ TestCase (assertEqual "basic" (lispSexprsToAsts [Lisp.SExpr.Number 11]) (Just [Lisp.Types.Number 11]))
        , TestCase (assertEqual "function with call" (lispSexprsToAsts [Lisp.SExpr.List [Lisp.SExpr.Symbol "define", Lisp.SExpr.Symbol "number", Lisp.SExpr.Number 1], Lisp.SExpr.Symbol "number"]) (Just [Lisp.Types.Call (Lisp.Types.Symbol "define") [Lisp.Types.Symbol "number", Lisp.Types.Number 1], Lisp.Types.Symbol "number"]))
        ]

testSExprShow :: Test
testSExprShow =
    TestList
        [ TestCase (assertEqual "basic" (show (Lisp.SExpr.Number 10)) "10")
        , TestCase (assertEqual "boolean true" (show (Lisp.SExpr.Symbol "#t")) "#t")
        , TestCase (assertEqual "boolean false" (show (Lisp.SExpr.Symbol "#f")) "#f")
        , TestCase (assertEqual "list" (show (Lisp.SExpr.List [Lisp.SExpr.Symbol "if", Lisp.SExpr.Symbol "#t", Lisp.SExpr.Number 1, Lisp.SExpr.Number 2])) "(if #t 1 2)")
        , TestCase (assertEqual "empty" (show (Lisp.SExpr.List [])) "()")
        , TestCase (assertEqual "list with one element" (show (Lisp.SExpr.List [Lisp.SExpr.Number 2])) "(2)")
        ]
