module Dreamberd.Types (AstNode (..)) where

data AstNode
    = Number Int
    | Boolean Bool
    | String String
    | Identifier String
    | Function String [String] [AstNode]
    | Call String [AstNode]
    | Operator String AstNode AstNode
    | AssignVariable String String AstNode
    | If AstNode [AstNode] [AstNode]
    | Return AstNode
    | Loop AstNode [AstNode] (Maybe AstNode) (Maybe AstNode)
    | List [AstNode]
    deriving (Eq)

instance Show AstNode where
    show (Number val) = show val
    show (Boolean True) = "true"
    show (Boolean False) = "false"
    show (String string) = show string
    show (Identifier identifier) = "(Identifier " ++ identifier ++ ")"
    show (Operator symbol leftElem rightElem) = "(Operator " ++ show symbol ++ " " ++ show leftElem ++ " " ++ show rightElem ++ ")"
    show (AssignVariable varType name value) = "(AssignVariable " ++ varType ++ " " ++ show name ++ " " ++ show value ++ ")"
    show (If test trueBody falseBody) = "If (" ++ show test ++ ") " ++ show trueBody ++ " " ++ show falseBody
    show (List []) = "()"
    show (List list) = "[" ++ unwords (map show list) ++ "]"
    show (Function name params body) = "Function " ++ show name ++ " " ++ show params ++ " " ++ show body
    show (Call name params) = "(Call " ++ name ++ " " ++ show params ++ ")"
    show (Loop test body initializer update) = "(Loop " ++ show test ++ " " ++ show body ++ " " ++ show initializer ++ " " ++ show update ++ ")"
    show (Return element) = "Return " ++ show element
