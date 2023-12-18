module SExpr (
    SymbolicExpression (..),
    sexprToAst,
    sexprsToAsts,
) where

import qualified Types (AstNode (..))

data SymbolicExpression
    = Number Integer
    | Symbol String
    | List [SymbolicExpression]

instance Show SymbolicExpression where
    show (Number val) = show val
    show (Symbol sym) = sym
    show (List []) = "()"
    show (List list) = "(" ++ unwords (map show list) ++ ")"

sexprToAst :: SExpr.SymbolicExpression -> Maybe Types.AstNode
sexprToAst (Number val) = Just (Types.Number val)
sexprToAst (Symbol "#t") = Just (Types.Boolean True)
sexprToAst (Symbol "#f") = Just (Types.Boolean False)
sexprToAst (Symbol "true") = Just (Types.Boolean True)
sexprToAst (Symbol "false") = Just (Types.Boolean False)
sexprToAst (Symbol sym) = Just (Types.Symbol sym)
sexprToAst (List [x]) = sexprToAst x >>= \callee -> Just $ Types.Call callee []
sexprToAst (List (x : xs)) = sexprToAst x >>= \callee -> Types.Call callee <$> mapM sexprToAst xs
sexprToAst (List []) = Just (Types.Symbol "()")

sexprsToAsts :: [SExpr.SymbolicExpression] -> Maybe [Types.AstNode]
sexprsToAsts = mapM sexprToAst
