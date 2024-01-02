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
    deriving (Show)

data Insts
    = Push Value
    | Call Ops
    | Ret

exec :: [Value] -> [Insts] -> Either String Value
exec _ [] = Left "No instructions"
exec [] (Ret:_) = Right Void
exec (value:_) (Ret:_) = Right value
exec stack (Push val:xs) = exec (val:stack) xs
exec stack (Call op:insts) = case execCall stack op of
    Left err -> Left err
    Right newValues -> exec newValues insts

execCall :: [Value] -> Ops -> Either String [Value]
execCall (Number 0:Number 0:_) Div = Left "Cannot divide by 0"
execCall (Number l:Number r:xs) op = case op of 
    Add -> Right (Number (r + l):xs)
    Sub -> Right (Number (r - l):xs)
    Mul -> Right (Number (r * l):xs)
    Div -> Right (Number (div r l):xs)
    _ -> Left ("Wrong data types in stack: " ++ show op ++ " with 2 numbers")
execCall _ _ = Left "Unknown operation"
