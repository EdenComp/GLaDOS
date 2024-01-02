module Vm (
    exec,
    Call(..),
    Insts(..),
    Value(..)
) where

data Value
    = Number Integer
    | Bool Bool
    | Symbol Call
    | Void
    deriving (Show)

data Call
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
    | Function [Char]
    deriving (Show)

data Insts
    = Push Value
    | PushArg Int
    | Call
    | JumpIfFalse Int
    | Ret
    deriving (Show)

exec :: [Value] -> [Value] -> [Insts] -> Either String Value
exec _ _ [] = Right Void
exec _ [] (Ret:_) = Right Void
exec _ (value:_) (Ret:_) = Right value
exec args stack (Push val:insts) = exec args (val:stack) insts
exec args stack (PushArg idx:insts)
    | idx >= length args = Left "Argument index out of bounds"
    | otherwise = exec args ((args !! idx):stack) insts
exec args stack (Call:insts) = case execCall stack of
    Left err -> Left err
    Right newValues -> exec args newValues insts
exec _ [] (x:_) = Left ("Stack is empty for a " ++ show x ++ " instruction")
exec args (Bool x:xs) (JumpIfFalse num:insts) 
    | num < 1 = Left "Invalid number of instructions"
    | num > length insts = Left "Cannot jump this amount of instructions"
    | not x = exec args xs (drop num insts)
    | otherwise = exec args xs insts
exec _ _ (JumpIfFalse _:_) = Left "Wrong data types in stack: JumpIfFalse needs a Bool"

execCall :: [Value] -> Either String [Value]
execCall [] = Left "Stack is empty for Call instruction"
execCall (Symbol sym:xs) = execBuiltin xs sym
execCall _ = Left "Stack argument is not a symbol"

execBuiltin :: [Value] -> Call -> Either String [Value]
execBuiltin (Number 0:Number 0:_) Div = Left "Cannot divide by 0"
execBuiltin (Number l:Number r:xs) op = case op of 
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
execBuiltin (Bool l:Bool r:xs) op = case op of
    Eq -> Right (Bool (l == r):xs)
    Neq -> Right (Bool (l /= r):xs)
    _ -> Left ("Wrong data types in stack: " ++ show op ++ " with 2 booleans")
execBuiltin _ (Function fct) = Left ("Executing function " ++ fct)
execBuiltin _ op = Left ("Wrong stack variables for builtin " ++ show op)
