module SExpr (
    SymbolicExpression (..),
    getSymbol,
    printTree,
) where

import Data.List (intercalate)

data SymbolicExpression
    = Number Float
    | Symbol String
    | List [SymbolicExpression]

instance Show SymbolicExpression where
    show (Number val) = "a Number " ++ show val
    show (Symbol sym) = "a Symbol '" ++ sym ++ "'"
    show (List []) = "an empty List"
    show (List list) = "a List containing " ++ intercalate ", followed by " (map show list)

getSymbol :: SymbolicExpression -> Maybe String
getSymbol (Symbol sym) = Just sym
getSymbol _ = Nothing

printTree :: SymbolicExpression -> Maybe String
printTree expr = Just (show expr)
