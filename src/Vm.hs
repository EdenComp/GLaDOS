module Vm (
    exec,
    Insts(..),
    Ops(..),
    Value(..)
) where

data Value
    = Number Integer
    | Bool Bool
    | Void
    deriving (Eq, Show)

data Ops
    = Add
    | Sub
    | Mul
    | Div
    | Eq
    | Neq
    | Less
    | LessOrEqual
    | Greater
    | GreaterOrEqual
    deriving (Show)

data Insts
    = Push Value
    | Call Ops
    | JumpIfFalse Int
    | Ret

exec :: [Value] -> [Insts] -> Either String Value
exec _ [] = Right Void
exec [] (Ret:_) = Right Void
exec (value:_) (Ret:_) = Right value
exec stack (Push val:insts) = exec (val:stack) insts
exec stack (Call op:insts) = case execCall stack op of
    Left err -> Left err
    Right newValues -> exec newValues insts
exec [] (JumpIfFalse _:_) = Left "Stack is empty"
exec (Bool x:xs) (JumpIfFalse num:insts) | num < 1 = Left "Invalid number of instructions"
                                         | num > length insts = Left "Cannot jump this amount of instructions"
                                         | not x = exec xs (drop num insts)
                                         | otherwise = exec xs insts
exec _ (JumpIfFalse _:_) = Left "Wrong data types in stack: JumpIfFalse needs a Bool"


execCall :: [Value] -> Ops -> Either String [Value]
execCall (Number 0:Number 0:_) Div = Left "Cannot divide by 0"
execCall (Number l:Number r:xs) op = case op of 
    Add -> Right (Number (l + r):xs)
    Sub -> Right (Number (l - r):xs)
    Mul -> Right (Number (l * r):xs)
    Div -> Right (Number (div l r):xs)
    Eq -> Right (Bool (l == r):xs)
    Neq -> Right (Bool (l /= r):xs)
    Less -> Right (Bool (l < r):xs)
    LessOrEqual -> Right (Bool (l <= r):xs)
    Greater -> Right (Bool (l > r):xs)
    GreaterOrEqual -> Right (Bool (l >= r):xs)
    _ -> Left ("Wrong data types in stack: " ++ show op ++ " with 2 numbers")
execCall (Bool l:Bool r:xs) op = case op of
    Eq -> Right (Bool (l == r):xs)
    Neq -> Right (Bool (l /= r):xs)
    _ -> Left ("Wrong data types in stack: " ++ show op ++ " with 2 booleans")
execCall _ _ = Left "Unknown operation"
