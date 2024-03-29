module Dreamberd.Types (AstNode (..), File (..)) where

data AstNode
    = Integer Int
    | Float Double
    | Boolean Bool
    | String String
    | Identifier String
    | Function String [String] [AstNode]
    | Lambda [String] [AstNode]
    | Call AstNode [AstNode]
    | If AstNode [AstNode] [AstNode]
    | Return (Maybe AstNode)
    | Loop AstNode [AstNode] (Maybe AstNode) (Maybe AstNode)
    | Scope [AstNode]
    | Import String
    deriving (Eq)

data File a = File String a

instance Show AstNode where
    show (Integer val) = show val
    show (Float val) = show val
    show (Boolean True) = "true"
    show (Boolean False) = "false"
    show (String string) = show string
    show (Identifier identifier) = "(Identifier " ++ identifier ++ ")"
    show (If test trueBody falseBody) = "If (" ++ show test ++ ") " ++ show trueBody ++ " " ++ show falseBody
    show (Scope stmts) = "{" ++ unwords (map show stmts) ++ "}"
    show (Function name params body) = "Function " ++ show name ++ " " ++ show params ++ " " ++ show body
    show (Lambda params body) = "Lambda (" ++ show params ++ " " ++ show body ++ ")"
    show (Call name params) = "(Call " ++ show name ++ " " ++ show params ++ ")"
    show (Loop test body initializer update) = "(Loop " ++ show test ++ " " ++ show body ++ " " ++ show initializer ++ " " ++ show update ++ ")"
    show (Return element) = "Return " ++ show element
    show (Import path) = "Import " ++ show path
