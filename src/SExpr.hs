module SExpr (
    SymbolicExpression (..),
    getSymbol,
    printTree,
) where

import Data.List (intercalate)

data SymbolicExpression
    = Integer Int
    | Float Float
    | Symbol String
    | List [SymbolicExpression]

instance Show SymbolicExpression where
    show (Integer value) = "an Integer " ++ show value
    show (Symbol sym) = "a Symbol '" ++ sym ++ "'"
    show (List []) = "an empty List"
    show (List list) = "a List containing " ++ intercalate ", followed by " (map show list)
    show (Float value) = "a Float " ++ show value

getSymbol :: SymbolicExpression -> Maybe String
getSymbol (Symbol sym) = Just sym
getSymbol _ = Nothing

printTree :: SymbolicExpression -> Maybe String
printTree expr = Just (show expr)