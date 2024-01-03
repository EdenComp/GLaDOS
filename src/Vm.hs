module Vm (
    exec,
    Call (..),
    EnvValue (..),
    Insts (..),
    Value (..)
) where

data Value
    = Number Integer
    | Bool Bool
    | Symbol Call
    | Void
    deriving (Show)

data EnvValue
    = Function [Insts]
    | Variable Value
    deriving (Show)

data Env = Env
    { identifier :: String
    , value :: EnvValue
    }
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
    | FunctionName String
    deriving (Show)

data Insts
    = Push Value
    | PushArg Int
    | PushEnv String
    | Call
    | DefineEnv String EnvValue
    | JumpIfFalse Int
    | Ret
    deriving (Show)

exec :: [Env] -> [Value] -> [Value] -> [Insts] -> Either String Value
exec _ _ _ [] = Right Void
exec _ _ [] (Ret : _) = Right Void
exec _ _ (val : _) (Ret : _) = Right val
exec env args stack (Push val : insts) = exec env args (val : stack) insts
exec env args stack (PushArg idx : insts)
    | idx >= length args = Left "Argument index out of bounds"
    | otherwise = exec env args ((args !! idx) : stack) insts
exec env args stack (PushEnv name : insts) = case findEnvValue name env of
    Just _ -> exec env args (Symbol (FunctionName name) : stack) insts
    Nothing -> Left ("Environment " ++ name ++ "does not exist")
exec env args stack (DefineEnv name var : insts) = exec (addEnvValue name var env) args stack insts
exec env args stack (Call : insts) = case execCall env stack of
    Left err -> Left err
    Right newValues -> exec env args newValues insts
exec _ _ [] (x : _) = Left ("Stack is empty for a " ++ show x ++ " instruction")
exec env args (Bool x : xs) (JumpIfFalse num : insts) 
    | num < 1 = Left "Invalid number of instructions"
    | num > length insts = Left "Cannot jump this amount of instructions"
    | not x = exec env args xs (drop num insts)
    | otherwise = exec env args xs insts
exec _ _ _ (JumpIfFalse _ : _) = Left "Wrong data types in stack: JumpIfFalse needs a Bool"

execCall :: [Env] -> [Value] -> Either String [Value]
execCall _ [] = Left "Stack is empty for Call instruction"
execCall env (Symbol (FunctionName fct) : xs) = case findEnvValue fct env of
    Just (Function insts) -> case exec env xs [] insts of
        Left err -> Left err
        Right val -> Right (val : xs)
    _ -> Left ("Environment " ++ fct ++ "does not exist")
execCall _ (Symbol sym : xs) = execBuiltin xs sym
execCall _ _ = Left "Stack argument is not a symbol"

execBuiltin :: [Value] -> Call -> Either String [Value]
execBuiltin (Number 0 : Number 0 : _) Div = Left "Cannot divide by 0"
execBuiltin (Number l : Number r : xs) op = case op of 
    Add -> Right (Number (l + r) : xs)
    Sub -> Right (Number (l - r) : xs)
    Mul -> Right (Number (l * r) : xs)
    Div -> Right (Number (div l r) : xs)
    Eq -> Right (Bool (l == r) : xs)
    Neq -> Right (Bool (l /= r) : xs)
    Less -> Right (Bool (l < r) : xs)
    LessOrEqual -> Right (Bool (l <= r) : xs)
    Greater -> Right (Bool (l > r) : xs)
    GreaterOrEqual -> Right (Bool (l >= r) : xs)
    _ -> Left ("Wrong data types in stack: " ++ show op ++ " with 2 numbers")
execBuiltin (Bool l : Bool r : xs) op = case op of
    Eq -> Right (Bool (l == r) : xs)
    Neq -> Right (Bool (l /= r) : xs)
    _ -> Left ("Wrong data types in stack: " ++ show op ++ " with 2 booleans")
execBuiltin _ op = Left ("Wrong stack variables for builtin " ++ show op)

findEnvValue :: String -> [Env] -> Maybe EnvValue
findEnvValue _ [] = Nothing
findEnvValue searchIdentifier (x : xs)
    | identifier x == searchIdentifier = Just $ value x
    | otherwise = findEnvValue searchIdentifier xs

addEnvValue :: String -> EnvValue -> [Env] -> [Env]
addEnvValue iden val (x : xs)
    | identifier x == iden = Env{identifier = iden, value = val} : xs
    | otherwise = x : addEnvValue iden val xs
addEnvValue iden val [] = [Env{identifier = iden, value = val}]
