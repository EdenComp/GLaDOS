module Dreamberd.Vm (
    exec,
    Call (..),
    EnvValue (..),
    Insts (..),
    Value (..),
) where

data Value
    = Number Int
    | Bool Bool
    | String String
    | Symbol Call
    | Void

instance Show Value where
    show (Number nbr) = show nbr
    show (Bool b) = show b
    show (String str) = str
    show (Symbol sym) = show sym
    show Void = ""

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

exec :: [Env] -> [Value] -> [Value] -> [Insts] -> IO (Either String Value)
exec _ _ _ [] = return (Right Void)
exec _ _ [] (Ret : _) = return (Right Void)
exec _ _ (val : _) (Ret : _) = return (Right val)
exec env args stack (Push val : insts) = exec env args (val : stack) insts
exec env args stack (PushArg idx : insts)
    | idx >= length args = return (Left "Argument index out of bounds")
    | otherwise = exec env args ((args !! idx) : stack) insts
exec env args stack (PushEnv name : insts) = case findEnvValue name env of
    Just _ -> exec env args (Symbol (FunctionName name) : stack) insts
    Nothing -> return (Left ("Environment " ++ name ++ "does not exist"))
exec env args stack (DefineEnv name var : insts) = exec (addEnvValue name var env) args stack insts
exec env args stack (Call : insts) = do
    ret <- execCall env stack
    case ret of
        Left err -> return (Left err)
        Right newValues -> exec env args newValues insts
exec _ _ [] (x : _) = return (Left ("Stack is empty for a " ++ show x ++ " instruction"))
exec env args (Bool x : xs) (JumpIfFalse num : insts)
    | num < 1 = return (Left "Invalid number of instructions")
    | num > length insts = return (Left "Cannot jump this amount of instructions")
    | not x = exec env args xs (drop num insts)
    | otherwise = exec env args xs insts
exec _ _ _ (JumpIfFalse _ : _) = return (Left "Wrong data types in stack: JumpIfFalse needs a Bool")

execCall :: [Env] -> [Value] ->  IO (Either String [Value])
execCall _ [] = return (Left "Stack is empty for Call instruction")
execCall _ (Symbol (FunctionName "print") : val : xs) = putStr (show val) >> return (Right xs)
execCall _ (Symbol (FunctionName "print") : _) = return (Left "Stack is empty for print instruction")
execCall env (Symbol (FunctionName fct) : xs) = case findEnvValue fct env of
    Just (Function insts) -> do
        ret <- exec env xs [] insts
        case ret of
            Left err -> return (Left err)
            Right val -> return (Right (val : xs))
    _ -> return (Left ("Environment " ++ fct ++ "does not exist"))
execCall _ (Symbol sym : xs) = return (execBuiltin xs sym)
execCall _ _ = return (Left "Stack argument is not a symbol")

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
execBuiltin (String str : Number nb : xs) op = case op of
    Add -> Right (String (str ++ show nb) : xs)
    Mul -> Right (String (concat $ replicate nb str) : xs)
    _ -> Left ("Wrong data types in stack: " ++ show op ++ " with a string and a number")
execBuiltin (String str : r : xs) op = case op of
    Add -> Right (String (str ++ show r) : xs)
    Eq -> Right (Bool (str == show r) : xs)
    Neq -> Right (Bool (str == show r) : xs)
    _ -> Left ("Wrong data types in stack: " ++ show op ++ " with a string and " ++ show r)
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
