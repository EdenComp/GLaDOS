module SExpr (
    SymbolicExpression (..),
    getSymbol,
    printTree,
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

getSymbol :: SymbolicExpression -> Maybe String
getSymbol (Symbol sym) = Just sym
getSymbol _ = Nothing

printTree :: SymbolicExpression -> Maybe String
printTree expr = Just (show expr)

sexprToAst :: SExpr.SymbolicExpression -> Maybe Types.AstNode
sexprToAst (Number val) = Just (Types.Number val)
sexprToAst (Symbol "#t") = Just (Types.Boolean True)
sexprToAst (Symbol "#f") = Just (Types.Boolean False)
sexprToAst (Symbol sym) = Just (Types.Symbol sym)
sexprToAst (List [x]) = sexprToAst x >>= \callee -> Just $ Types.Call callee []
sexprToAst (List (x : xs)) = sexprToAst x >>= \callee -> Types.Call callee <$> mapM sexprToAst xs
sexprToAst (List []) = Just (Types.Symbol "()")

sexprsToAsts :: [SExpr.SymbolicExpression] -> Maybe [Types.AstNode]
sexprsToAsts = mapM sexprToAst
