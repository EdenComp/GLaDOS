{-# LANGUAGE RankNTypes #-}

module Types (
    AstNode (..),
    Variable (..),
    NodeEvaluator,
    BuiltinOperator,
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
    deriving (Show)

type NodeEvaluator = [Variable] -> AstNode -> Maybe (AstNode, [Variable])
type BuiltinOperator = forall b. (Integral b) => b -> b -> b
