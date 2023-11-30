module Ast (
  Ast (..),
  sexprToAst,
  runAst,
) where

import qualified SExpr (SymbolicExpression (Float, Integer, List, Symbol))

data Ast
  = Define String Int
  | Integer Int
  | Float Float
  | Symbol String
  | Boolean Bool
  | Call String Int Int
  deriving (Show)

sexprToAst :: SExpr.SymbolicExpression -> Maybe Ast
sexprToAst (SExpr.Integer value) = Just (Integer value)
sexprToAst (SExpr.Float value) = Just (Float value)
sexprToAst (SExpr.Symbol "#t") = Just (Boolean True)
sexprToAst (SExpr.Symbol "#f") = Just (Boolean False)
sexprToAst (SExpr.Symbol sym) = Just (Symbol sym)
sexprToAst (SExpr.List [SExpr.Symbol "define", SExpr.Symbol name, SExpr.Integer value]) = Just (Define name value)
sexprToAst (SExpr.List [SExpr.Symbol op, SExpr.Integer left, SExpr.Integer right])
  | op == "+" || op == "*" = Just (Call op left right)
  | otherwise = Nothing
sexprToAst _ = Nothing

runAst :: Ast -> Maybe Ast
runAst (Call "+" l r) = Just (Integer (l + r))
runAst (Call "*" l r) = Just (Integer (l * r))
runAst _ = Nothing