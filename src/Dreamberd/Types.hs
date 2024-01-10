module Dreamberd.Types (AstNode (..)) where

data AstNode
    = Number Int
    | Boolean Bool
    | String String
    | Identifier String
    | Function String [String] [AstNode]
    | Call String [AstNode]
    | If AstNode [AstNode] [AstNode]
    | Return AstNode
    | Loop AstNode [AstNode] (Maybe AstNode) (Maybe AstNode)
    | List [AstNode]

instance Show AstNode where
    show (Number val) = show val
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

instance Eq AstNode where
    (Number a) == (Number b) = a == b
    (Boolean a) == (Boolean b) = a == b
    (String a) == (String b) = a == b
    (Identifier a) == (Identifier b) = a == b
    (Operator a b c) == (Operator d e f) = a == d && b == e && c == f
    (If a b c) == (If d e f) = a == d && b == e && c == f
    (List a) == (List b) = a == b
    (Function a b c) == (Function d e f) = a == d && b == e && c == f
    (Call a b) == (Call d e) = a == d && b == e
    (Loop a b c d) == (Loop e f g h) = a == e && b == f && c == g && d == h
    (Return a) == (Return b) = a == b
    _ == _ = False

instance Ord AstNode where
    (Number a) `compare` (Number b) = a `compare` b
    (Boolean a) `compare` (Boolean b) = a `compare` b
    (String a) `compare` (String b) = a `compare` b
    (Identifier a) `compare` (Identifier b) = a `compare` b
    (Operator a b c) `compare` (Operator d e f) = a `compare` d <> b `compare` e <> c `compare` f
    (If a b c) `compare` (If d e f) = a `compare` d <> b `compare` e <> c `compare` f
    (List a) `compare` (List b) = a `compare` b
    (Function a b c) `compare` (Function d e f) = a `compare` d <> b `compare` e <> c `compare` f
    (Call a b) `compare` (Call d e) = a `compare` d <> b `compare` e
    (Loop a b c d) `compare` (Loop e f g h) = a `compare` e <> b `compare` f <> c `compare` g <> d `compare` h
    (Return a) `compare` (Return b) = a `compare` b
    _ `compare` _ = EQ

instance Num AstNode where
    (Number a) + (Number b) = Number (a + b)
    _ + _ = error "Cannot add non-number values"
    (Number a) - (Number b) = Number (a - b)
    _ - _ = error "Cannot subtract non-number values"
    (Number a) * (Number b) = Number (a * b)
    _ * _ = error "Cannot multiply non-number values"
    abs (Number a) = Number (abs a)
    abs _ = error "Cannot get absolute value of non-number value"
    signum (Number a) = Number (signum a)
    signum _ = error "Cannot get signum of non-number value"
    fromInteger a = Number (fromInteger a)

instance Integral AstNode where
    toInteger (Number a) = toInteger a
    toInteger _ = error "Cannot convert non-number value to integer"
    quotRem (Number a) (Number b) = (Number (quot a b), Number (rem a b))
    quotRem _ _ = error "Cannot divide non-number values"

instance Real AstNode where
    toRational (Number a) = toRational a
    toRational _ = error "Cannot convert non-number value to rational"

instance Enum AstNode where
    toEnum a = Number (toEnum a)
    fromEnum (Number a) = fromEnum a
    fromEnum _ = error "Cannot convert non-number value from enum"
