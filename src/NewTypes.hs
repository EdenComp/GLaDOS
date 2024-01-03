module NewTypes (AstNode (..)) where

data AstNode
    = Number Integer
    | Boolean Bool
    | String String
    | Identifier String
    | Function String [String] [AstNode]
    | Call String [AstNode]
    | Operator String AstNode AstNode
    | If AstNode [AstNode] [AstNode]
    | Return AstNode
    | List [AstNode]

instance Show AstNode where
    show (Number val) = show val
    show (Boolean True) = "true"
    show (Boolean False) = "false"
    show (String string) = show string
    show (Identifier identifier) = "id:" ++ show identifier
    show (Operator symbol leftElem rightElem) = "(" ++ show leftElem ++ show symbol ++ show rightElem ++ ")"
    show (If test trueBody falseBody) = "if (" ++ show test ++ ") {" ++ show trueBody ++ "} else {" ++ show falseBody ++ "}"
    show (List []) = "()"
    show (List list) = "[" ++ unwords (map show list) ++ "]"
    show (Function name params body) = "function " ++ show name ++ "(" ++ unwords (map show params) ++ ") {" ++ show body ++ "}"
    show (Call name params) = show name ++ "(" ++ show params ++ ")"
    show (Return element) = show element
