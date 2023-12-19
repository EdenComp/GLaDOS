{-# LANGUAGE RankNTypes #-}

module Types (
    AstNode (..),
    Variable (..),
    NodeEvaluator,
    BuiltinOperator,
    BuiltinComparisonOperator,
) where

data Variable = Variable
    { identifier :: String
    , value :: AstNode
    }
    deriving (Show)

data AstNode
    = Number Integer
    | Symbol String
    | Boolean Bool
    | Call AstNode [AstNode]
    | Lambda [String] AstNode
    | Void
    deriving (Eq, Show)

-- instance Show AstNode where
--     show (Number val) = show val
--     show (Symbol sym) = sym
--     show (Call _ _) = "#<procedure>"
--     show (Lambda _ _) = "#<procedure>"
--     show (Boolean True) = "#t"
--     show (Boolean False) = "#f"
--     show Void = undefined

type NodeEvaluator = [Variable] -> AstNode -> Maybe (AstNode, [Variable])
type BuiltinOperator = forall b. (Integral b) => b -> b -> b
type BuiltinComparisonOperator = forall b. (Integral b) => b -> b -> Bool
