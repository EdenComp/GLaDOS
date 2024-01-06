module Lisp.SExpr (
    SymbolicExpression (..),
    sexprToAst,
    lispSexprsToAsts,
) where

import qualified Lisp.Types (AstNode (..))

data SymbolicExpression
    = Number Integer
    | Symbol String
    | List [SymbolicExpression]

instance Show SymbolicExpression where
    show (Number val) = show val
    show (Symbol sym) = sym
    show (List []) = "()"
    show (List list) = "(" ++ unwords (map show list) ++ ")"

sexprToAst :: SymbolicExpression -> Maybe Lisp.Types.AstNode
sexprToAst (Number val) = Just (Lisp.Types.Number val)
sexprToAst (Symbol "#t") = Just (Lisp.Types.Boolean True)
sexprToAst (Symbol "#f") = Just (Lisp.Types.Boolean False)
sexprToAst (Symbol "true") = Just (Lisp.Types.Boolean True)
sexprToAst (Symbol "false") = Just (Lisp.Types.Boolean False)
sexprToAst (Symbol sym) = Just (Lisp.Types.Symbol sym)
sexprToAst (List [x]) = sexprToAst x >>= \callee -> Just $ Lisp.Types.Call callee []
sexprToAst (List (x : xs)) = sexprToAst x >>= \callee -> Lisp.Types.Call callee <$> mapM sexprToAst xs
sexprToAst (List []) = Just (Lisp.Types.Symbol "()")

lispSexprsToAsts :: [SymbolicExpression] -> Maybe [Lisp.Types.AstNode]
lispSexprsToAsts = mapM sexprToAst
