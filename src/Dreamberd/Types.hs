module Dreamberd.Types (AstNode (..), File (..)) where

data AstNode
    = Number Int
    | Float Float
    | Boolean Bool
    | String String
    | Identifier String
    | Function String [String] [AstNode]
    | Call String [AstNode]
    | If AstNode [AstNode] [AstNode]
    | Return AstNode
    | Loop AstNode [AstNode] (Maybe AstNode) (Maybe AstNode)
    | List [AstNode]
    deriving (Eq)

data File a = File String a

instance Show AstNode where
    show (Number val) = show val
    show (Float val) = show val
    show (Boolean True) = "true"
    show (Boolean False) = "false"
    show (String string) = show string
    show (Identifier identifier) = "(Identifier " ++ identifier ++ ")"
    show (If test trueBody falseBody) = "If (" ++ show test ++ ") " ++ show trueBody ++ " " ++ show falseBody
    show (List list) = "[" ++ unwords (map show list) ++ "]"
    show (Function name params body) = "Function " ++ show name ++ " " ++ show params ++ " " ++ show body
    show (Call name params) = "(Call " ++ name ++ " " ++ show params ++ ")"
    show (Loop test body initializer update) = "(Loop " ++ show test ++ " " ++ show body ++ " " ++ show initializer ++ " " ++ show update ++ ")"
    show (Return element) = "Return " ++ show element
