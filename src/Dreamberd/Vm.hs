module Dreamberd.Vm (
    exec,
    execVM,
    Call (..),
    DefineEnvType (..),
    Env (..),
    EnvValue (..),
    Insts (..),
    Operator (..),
    Value (..),
) where

import Data.Fixed (mod')
import GHC.Float (int2Double)
import System.IO (hFlush, hPutStr, stderr, stdout)

data Value
    = Integer Int
    | Float Double
    | Bool Bool
    | String String
    | Symbol Call
    | Lambda [Insts]
    | Void
    deriving (Eq)

instance Show Value where
    show (Integer nbr) = show nbr
    show (Float nbr) = show nbr
    show (Bool b) = show b
    show (String str) = str
    show (Symbol sym) = show sym
    show (Lambda _) = "<lambda>"
    show Void = ""

data EnvValue
    = Function Int [Insts]
    | Variable Value
    deriving (Eq, Show)

data Env = Env
    { identifier :: String
    , value :: EnvValue
    , scope :: Int
    }
    deriving (Show)

data DefineEnvType
    = Define
    | Redefine
    | Override
    deriving (Eq, Show)

data Call
    = Builtin Operator
    | FunctionName String
    deriving (Eq, Show)

data Operator
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | Pow
    | Eq
    | Neq
    | Less
    | LessOrEqual
    | Greater
    | GreaterOrEqual
    | And
    | Or
    | Xor
    deriving (Enum, Eq, Show)

data Insts
    = Push Value
    | PushArg Int
    | PushEnv String
    | Call
    | DefineEnv String DefineEnvType (Maybe EnvValue)
    | EraseEnv String
    | Jump Int (Maybe Bool)
    | Ret
    deriving (Eq, Show)

execVM :: [Insts] -> IO (Either String Value)
execVM insts = exec [] [] [] insts 0 0

exec :: [Env] -> [Value] -> [Value] -> [Insts] -> Int -> Int -> IO (Either String Value)
exec _ _ _ [] _ _ = return (Right Void)
exec env args stack insts idx scopeIdx
    | idx < 0 || idx > length insts = return (Left "Instructions index out of bounds")
    | idx == length insts = return (Right Void)
    | otherwise = execInstruction env args stack insts (insts !! idx) idx scopeIdx

execInstruction :: [Env] -> [Value] -> [Value] -> [Insts] -> Insts -> Int -> Int -> IO (Either String Value)
execInstruction _ _ [] _ Ret _ _ = return (Right Void)
execInstruction _ _ (val : _) _ Ret _ _ = return (Right val)
execInstruction env args stack insts (Push val) idx scopeIdx = exec env args (val : stack) insts (idx + 1) scopeIdx
execInstruction env args stack insts (PushArg arg) idx scopeIdx
    | arg >= length args || arg < 0 = return (Left "Argument index out of bounds")
    | otherwise = exec env args ((args !! arg) : stack) insts (idx + 1) scopeIdx
execInstruction env args stack insts (PushEnv "input") idx scopeIdx = exec env args (Symbol (FunctionName "input") : stack) insts (idx + 1) scopeIdx
execInstruction env args stack insts (PushEnv "print") idx scopeIdx = exec env args (Symbol (FunctionName "print") : stack) insts (idx + 1) scopeIdx
execInstruction env args stack insts (PushEnv "error") idx scopeIdx = exec env args (Symbol (FunctionName "error") : stack) insts (idx + 1) scopeIdx
execInstruction env args stack insts (PushEnv name) idx scopeIdx =
    case findEnvValue name env of
        Just (Function _ _, _) -> exec env args (Symbol (FunctionName name) : stack) insts (idx + 1) scopeIdx
        Just (Variable v, _) -> exec env args (v : stack) insts (idx + 1) scopeIdx
        _ -> return (Left ("Environment " ++ name ++ " does not exist"))
execInstruction env args stack insts (EraseEnv name) idx scopeIdx = case findEnvValue name env of
    Just _ -> exec (removeEnvValue name env) args stack insts (idx + 1) scopeIdx
    Nothing -> return (Left ("Environment " ++ name ++ " does not exist"))
execInstruction env args stack insts (DefineEnv name redef val) idx scopeIdx = execDefineEnv env args stack insts idx scopeIdx name redef val
execInstruction env args stack insts Call idx scopeIdx = do
    ret <- execCall env stack scopeIdx
    case ret of
        Left err -> return (Left err)
        Right newValues -> exec env args newValues insts (idx + 1) scopeIdx
execInstruction env args stack insts (Jump num cond) idx scopeIdx = execJump env args stack insts idx scopeIdx num cond

execCall :: [Env] -> [Value] -> Int -> IO (Either String [Value])
execCall _ [] _ = return (Left "Stack is empty for a Call instruction")
execCall _ (Symbol (FunctionName "input") : xs) _ = hFlush stdout >> hFlush stderr >> getLine >>= \line -> return (Right (String line : xs))
execCall _ (Symbol (FunctionName "print") : val : xs) _ = putStr (show val) >> return (Right xs)
execCall _ (Symbol (FunctionName "print") : _) _ = return (Left "Stack is empty for a print instruction")
execCall _ (Symbol (FunctionName "error") : val : xs) _ = hPutStr stderr (show val) >> return (Right xs)
execCall _ (Symbol (FunctionName "error") : _) _ = return (Left "Stack is empty for an error instruction")
execCall env (Symbol (FunctionName fct) : xs) scopeIdx = case findEnvValue fct env of
    Just (Function args insts, fctScope) -> do
        ret <- exec (filter (\e -> scope e <= fctScope) env) xs [] insts 0 (scopeIdx + 1)
        case ret of
            Left err -> return (Left err)
            Right val -> return (Right (val : drop args xs))
    _ -> return (Left ("Environment " ++ fct ++ " does not exist"))
execCall _ (Symbol (Builtin op) : xs) _ = return (execBuiltin xs op)
execCall env (Lambda insts : xs) scopeIdx = do
    ret <- exec env xs [] insts 0 (scopeIdx + 1)
    case ret of
        Left err -> return (Left err)
        Right val -> return (Right (val : xs))
execCall _ _ _ = return (Left "Stack argument is not a symbol or a lambda")

execJump :: [Env] -> [Value] -> [Value] -> [Insts] -> Int -> Int -> Int -> Maybe Bool -> IO (Either String Value)
execJump _ _ _ _ _ _ (-1) _ = return (Left "Invalid number of instructions")
execJump _ _ _ insts idx _ num _ | num > 0 && num >= (length insts - idx) = return (Left "Invalid number of instructions")
execJump _ _ _ _ idx _ num _ | num < (idx + 1) * (-1) = return (Left "Invalid number of instructions")
execJump env args stack insts idx scopeIdx num Nothing = exec env args stack insts (idx + num + 1) scopeIdx
execJump env args (x : xs) insts idx scopeIdx num (Just b)
    | toBool x == b && num == length insts - idx = return $ Right Void
    | toBool x == b = exec env args xs insts (idx + num + 1) scopeIdx
    | otherwise = exec env args xs insts (idx + 1) scopeIdx
execJump _ _ _ _ _ _ _ _ = return (Left "Stack is empty for a conditional jump")

execDefineEnv :: [Env] -> [Value] -> [Value] -> [Insts] -> Int -> Int -> String -> DefineEnvType -> Maybe EnvValue -> IO (Either String Value)
execDefineEnv _ _ [] _ _ _ _ _ Nothing = return (Left "Stack is empty for a DefineEnv from stack instruction")
execDefineEnv env args (x : xs) insts idx scopeIdx name Define Nothing =
    case findEnvValue name env of
        Just _ -> return (Left ("Environment " ++ name ++ " already exists"))
        Nothing -> exec (addEnvValue name (Variable x) scopeIdx env) args xs insts (idx + 1) scopeIdx
execDefineEnv env args stack insts idx scopeIdx name Define (Just val) =
    case findEnvValue name env of
        Just _ -> return (Left ("Environment " ++ name ++ " already exists"))
        Nothing -> exec (addEnvValue name val scopeIdx env) args stack insts (idx + 1) scopeIdx
execDefineEnv env args (x : xs) insts idx scopeIdx name Redefine Nothing =
    case findEnvValue name env of
        Just (_, initialScope) -> exec (addEnvValue name (Variable x) initialScope env) args xs insts (idx + 1) scopeIdx
        Nothing -> return (Left ("Environment " ++ name ++ " does not exist"))
execDefineEnv env args stack insts idx scopeIdx name Redefine (Just val) =
    case findEnvValue name env of
        Just (_, initialScope) -> exec (addEnvValue name val initialScope env) args stack insts (idx + 1) scopeIdx
        Nothing -> return (Left ("Environment " ++ name ++ " does not exist"))
execDefineEnv env args (x : xs) insts idx scopeIdx name Override Nothing =
    exec (addEnvValue name (Variable x) scopeIdx env) args xs insts (idx + 1) scopeIdx
execDefineEnv env args stack insts idx scopeIdx name Override (Just val) =
    exec (addEnvValue name val scopeIdx env) args stack insts (idx + 1) scopeIdx

execBuiltin :: [Value] -> Operator -> Either String [Value]
execBuiltin (Integer _ : Integer 0 : _) Div = Left "Cannot divide by 0"
execBuiltin (Integer _ : Integer 0 : _) Mod = Left "Cannot divide by 0"
execBuiltin (Float _ : Float 0 : _) Div = Left "Cannot divide by 0"
execBuiltin (Float _ : Float 0 : _) Mod = Left "Cannot divide by 0"
execBuiltin (Integer l : Integer r : xs) op = case op of
    Add -> Right (Integer (l + r) : xs)
    Sub -> Right (Integer (l - r) : xs)
    Mul -> Right (Integer (l * r) : xs)
    Div -> Right (Integer (div l r) : xs)
    Mod -> Right (Integer (mod l r) : xs)
    Pow -> Right (Integer (l ^ r) : xs)
    Eq -> Right (Bool (l == r) : xs)
    Neq -> Right (Bool (l /= r) : xs)
    Less -> Right (Bool (l < r) : xs)
    LessOrEqual -> Right (Bool (l <= r) : xs)
    Greater -> Right (Bool (l > r) : xs)
    GreaterOrEqual -> Right (Bool (l >= r) : xs)
    _ -> Left ("Wrong data types in stack: " ++ show op ++ " with 2 integers")
execBuiltin (Float l : Float r : xs) op = case op of
    Add -> Right (Float (l + r) : xs)
    Sub -> Right (Float (l - r) : xs)
    Mul -> Right (Float (l * r) : xs)
    Div -> Right (Float (l / r) : xs)
    Mod -> Right (Float (mod' l r) : xs)
    Pow -> Right (Float (l ** r) : xs)
    Eq -> Right (Bool (l == r) : xs)
    Neq -> Right (Bool (l /= r) : xs)
    Less -> Right (Bool (l < r) : xs)
    LessOrEqual -> Right (Bool (l <= r) : xs)
    Greater -> Right (Bool (l > r) : xs)
    GreaterOrEqual -> Right (Bool (l >= r) : xs)
    _ -> Left ("Wrong data types in stack: " ++ show op ++ " with 2 floats")
execBuiltin (Float l : Integer r : xs) op = case op of
    Add -> Right (Float (l + int2Double r) : xs)
    Sub -> Right (Float (l - int2Double r) : xs)
    Mul -> Right (Float (l * int2Double r) : xs)
    Div -> Right (Float (l / int2Double r) : xs)
    Mod -> Right (Float (mod' l (int2Double r)) : xs)
    Pow -> Right (Float (l ** int2Double r) : xs)
    Eq -> Right (Bool (l == int2Double r) : xs)
    Neq -> Right (Bool (l /= int2Double r) : xs)
    Less -> Right (Bool (l < int2Double r) : xs)
    LessOrEqual -> Right (Bool (l <= int2Double r) : xs)
    Greater -> Right (Bool (l > int2Double r) : xs)
    GreaterOrEqual -> Right (Bool (l >= int2Double r) : xs)
    _ -> Left ("Wrong data types in stack: " ++ show op ++ " with a float and an integer")
execBuiltin (Integer l : Float r : xs) op = execBuiltin (Float (int2Double l) : Float r : xs) op
execBuiltin (Bool l : Bool r : xs) op = case op of
    Eq -> Right (Bool (l == r) : xs)
    Neq -> Right (Bool (l /= r) : xs)
    And -> Right (Bool (l && r) : xs)
    Or -> Right (Bool (l || r) : xs)
    Xor -> Right (Bool (l /= r) : xs)
    _ -> Left ("Wrong data types in stack: " ++ show op ++ " with 2 booleans")
execBuiltin (String str : Integer nb : xs) op = case op of
    Add -> Right (String (str ++ show nb) : xs)
    Mul -> Right (String (concat $ replicate nb str) : xs)
    _ -> Left ("Wrong data types in stack: " ++ show op ++ " with a string and an integer")
execBuiltin (String str : r : xs) op = case op of
    Add -> Right (String (str ++ show r) : xs)
    Eq -> Right (Bool (str == show r) : xs)
    Neq -> Right (Bool (str /= show r) : xs)
    _ -> Left ("Wrong data types in stack: " ++ show op ++ " with a string as left operator")
execBuiltin (l : String str : xs) Add = Right (String (show l ++ str) : xs)
execBuiltin _ op = Left ("Wrong stack variables for builtin " ++ show op)

findEnvValue :: String -> [Env] -> Maybe (EnvValue, Int)
findEnvValue _ [] = Nothing
findEnvValue searchIdentifier (x : xs)
    | identifier x == searchIdentifier = Just (value x, scope x)
    | otherwise = findEnvValue searchIdentifier xs

addEnvValue :: String -> EnvValue -> Int -> [Env] -> [Env]
addEnvValue iden val scopeIdx (x : xs)
    | identifier x == iden = Env{identifier = iden, value = val, scope = scopeIdx} : xs
    | otherwise = x : addEnvValue iden val scopeIdx xs
addEnvValue iden val scopeIdx [] = [Env{identifier = iden, value = val, scope = scopeIdx}]

removeEnvValue :: String -> [Env] -> [Env]
removeEnvValue iden = filter (\x -> identifier x /= iden)

toBool :: Value -> Bool
toBool (Bool False) = False
toBool (Integer 0) = False
toBool (String "") = False
toBool Void = False
toBool _ = True
