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
    show (Identifier identifier) = "(Identifier " ++ identifier ++ ")"
    show (Operator symbol leftElem rightElem) = "(Operator " ++ show symbol ++ " " ++ show leftElem ++ " " ++ show rightElem ++ ")"
    show (If test trueBody falseBody) = "if (" ++ show test ++ ") {" ++ show trueBody ++ "} else {" ++ show falseBody ++ "}"
    show (List []) = "()"
    show (List list) = "[" ++ unwords (map show list) ++ "]"
    show (Function name params body) = "Function " ++ show name ++ " " ++ show params ++ " " ++ show body
    show (Call name params) = "(Call " ++ name ++ " " ++ show params ++ ")"
    show (Return element) = "Return " ++ show element
