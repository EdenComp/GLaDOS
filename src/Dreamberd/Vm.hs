module Dreamberd.Vm (
    exec,
    execVM,
    Call (..),
    Env (..),
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
    deriving (Eq)

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
    deriving (Eq, Show)

data Insts
    = Push Value
    | PushArg Int
    | PushEnv String
    | Call
    | DefineEnv String EnvValue
    | DefineEnvFromStack String
    | JumpIfFalse Int
    | Ret
    deriving (Show)

execVM :: [Insts] -> IO (Either String Value)
execVM insts = exec [] [] [] insts 0

exec :: [Env] -> [Value] -> [Value] -> [Insts] -> Int -> IO (Either String Value)
exec _ _ _ [] _ = return (Right Void)
exec env args stack insts idx | idx < 0 || idx > length insts = return (Left "Instructions index out of bounds")
                              | idx == length insts = return (Right Void)
                              | otherwise = execInstruction env args stack insts (insts !! idx) idx

execInstruction :: [Env] -> [Value] -> [Value] -> [Insts] -> Insts -> Int -> IO (Either String Value)
execInstruction _ _ [] _ Ret _ = return (Right Void)
execInstruction _ _ (val : _) _ Ret _ = return (Right val)
execInstruction env args stack insts (Push val) idx = exec env args (val : stack) insts (idx + 1)
execInstruction env args stack insts (PushArg arg) idx
    | arg >= length args || arg < 0 = return (Left "Argument index out of bounds")
    | otherwise = exec env args ((args !! (length args - arg - 1)) : stack) insts (idx + 1)
execInstruction env args stack insts (PushEnv name) idx = case findEnvValue name env of
    Just (Function _) -> exec env args (Symbol (FunctionName name) : stack) insts (idx + 1)
    Just (Variable v) -> exec env args (v : stack) insts (idx + 1)
    Nothing -> return (Left ("Environment " ++ name ++ " does not exist"))
execInstruction env args stack insts (DefineEnv name var) idx = exec (addEnvValue name var env) args stack insts (idx + 1)
execInstruction env args (x : xs) insts (DefineEnvFromStack name) idx = exec (addEnvValue name (Variable x) env) args xs insts (idx + 1)
execInstruction env args stack insts Call idx = do
    ret <- execCall env stack
    case ret of
        Left err -> return (Left err)
        Right newValues -> exec env args newValues insts (idx + 1)
execInstruction _ _ [] _ x _ = return (Left ("Stack is empty for a " ++ show x ++ " instruction"))
execInstruction env args (Bool x : xs) insts (JumpIfFalse num) idx
    | num == (-1) || num > 0 && num >= (length insts - idx - 1) = return (Left "Invalid number of instructions")
    | num < idx * (-1) = return (Left "Invalid number of instructions")
    | not x = exec env args xs insts (idx + num + 1)
    | otherwise = exec env args xs insts (idx + 1)
execInstruction _ _ _ _ (JumpIfFalse _) _ = return (Left "Wrong data types in stack: JumpIfFalse needs a Bool")

execCall :: [Env] -> [Value] -> IO (Either String [Value])
execCall _ [] = return (Left "Stack is empty for a Call instruction")
execCall _ (Symbol (FunctionName "print") : val : xs) = putStr (show val) >> return (Right xs)
execCall _ (Symbol (FunctionName "print") : _) = return (Left "Stack is empty for print instruction")
execCall env (Symbol (FunctionName fct) : xs) = case findEnvValue fct env of
    Just (Function insts) -> do
        ret <- exec env xs [] insts 0
        case ret of
            Left err -> return (Left err)
            Right val -> return (Right (val : xs))
    _ -> return (Left ("Environment " ++ fct ++ " does not exist"))
execCall _ (Symbol sym : xs) = return (execBuiltin xs sym)
execCall _ _ = return (Left "Stack argument is not a symbol")

execBuiltin :: [Value] -> Call -> Either String [Value]
execBuiltin (Number _ : Number 0 : _) Div = Left "Cannot divide by 0"
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
