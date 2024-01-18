{-# LANGUAGE LambdaCase #-}

module Dreamberd.Vm (
    exec,
    execVM,
    Call (..),
    DefineEnvType (..),
    Env (..),
    Insts (..),
    Operator (..),
    Value (..),
    Variable (..),
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
    | Lambda Int [Insts]
    | Void
    deriving (Eq)

instance Show Value where
    show (Integer nbr) = show nbr
    show (Float nbr) = show nbr
    show (Bool b) = show b
    show (String str) = str
    show (Symbol sym) = show sym
    show (Lambda _ _) = "<lambda>"
    show Void = ""

data Variable = Variable Value Int
    deriving (Eq, Show)

data Env = Env 
    { identifier :: String
    , value :: Variable
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
    | DefineEnv String DefineEnvType (Maybe Value)
    | EraseEnv String
    | Jump Int (Maybe Bool)
    | Ret
    deriving (Eq, Show)

execVM :: [Insts] -> IO (Either String Value)
execVM insts = exec [] [] [] insts 0 0 >>= \case
    Left err -> return (Left err)
    Right (Variable val _) -> return (Right val)

exec :: [Env] -> [Variable] -> [Variable] -> [Insts] -> Int -> Int -> IO (Either String Variable)
exec _ _ _ [] _ scopeIdx = return $ Right (Variable Void scopeIdx)
exec env args stack insts idx scopeIdx
    | idx < 0 || idx > length insts = return (Left "Instructions index out of bounds")
    | idx == length insts = return $ Right (Variable Void scopeIdx)
    | otherwise = execInstruction env args stack insts (insts !! idx) idx scopeIdx

execInstruction :: [Env] -> [Variable] -> [Variable] -> [Insts] -> Insts -> Int -> Int -> IO (Either String Variable)
execInstruction _ _ [] _ Ret _ scopeIdx = return $ Right (Variable Void scopeIdx)
execInstruction _ _ (val : _) _ Ret _ _ = return $ Right val
execInstruction env args stack insts (Push val) idx scopeIdx = exec env args (Variable val scopeIdx : stack) insts (idx + 1) scopeIdx
execInstruction env args stack insts (PushArg arg) idx scopeIdx
    | arg >= length args || arg < 0 = return (Left "Argument index out of bounds")
    | otherwise = exec env args ((args !! arg) : stack) insts (idx + 1) scopeIdx
execInstruction env args stack insts (PushEnv "input") idx scopeIdx = exec env args (Variable (Symbol (FunctionName "input")) scopeIdx : stack) insts (idx + 1) scopeIdx
execInstruction env args stack insts (PushEnv "print") idx scopeIdx = exec env args (Variable (Symbol (FunctionName "print")) scopeIdx : stack) insts (idx + 1) scopeIdx
execInstruction env args stack insts (PushEnv "error") idx scopeIdx = exec env args (Variable (Symbol (FunctionName "error")) scopeIdx : stack) insts (idx + 1) scopeIdx
execInstruction env args stack insts (PushEnv name) idx scopeIdx =
    case findEnvValue name env of
        Just v -> exec env args (v : stack) insts (idx + 1) scopeIdx
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

execCall :: [Env] -> [Variable] -> Int -> IO (Either String [Variable])
execCall _ [] _ = return (Left "Stack is empty for a Call instruction")
execCall _ (Variable (Symbol (FunctionName "input")) _ : xs) scopeIdx = hFlush stdout >> hFlush stderr >> getLine >>= \line -> return $ Right (Variable (String line) scopeIdx : xs)
execCall _ (Variable (Symbol (FunctionName "print")) _ : (Variable val _) : xs) _ = putStr (show val) >> return (Right xs)
execCall _ (Variable (Symbol (FunctionName "print")) _ : _) _ = return (Left "Stack is empty for a print instruction")
execCall _ (Variable (Symbol (FunctionName "error")) _ : (Variable val _) : xs) _ = hPutStr stderr (show val) >> return (Right xs)
execCall _ (Variable (Symbol (FunctionName "error")) _ : _) _ = return (Left "Stack is empty for an error instruction")
execCall env (Variable (Symbol (FunctionName fct)) _ : xs) scopeIdx = case findEnvValue fct env of
    Just (Variable (Lambda args insts) fctScope) -> do
        ret <- exec (filter (\(Env _ (Variable _ scope)) -> scope <= fctScope) env) xs [] insts 0 (scopeIdx + 1)
        case ret of
            Left err -> return (Left err)
            Right val -> return (Right (val : drop args xs))
    _ -> return (Left ("Environment " ++ fct ++ " does not exist"))
execCall _ (Variable (Symbol (Builtin op)) _ : (Variable l _) : (Variable r _) : xs) scopeIdx = case execOperation l r op of
    Left err -> return (Left err)
    Right res -> return (Right (Variable res scopeIdx : xs))
execCall env (Variable (Lambda args insts) fctScope : xs) scopeIdx = do
    ret <- exec (filter (\(Env _ (Variable _ scope)) -> scope <= fctScope) env) xs [] insts 0 (scopeIdx + 1)
    case ret of
        Left err -> return (Left err)
        Right val -> return (Right (val : drop args xs))
execCall _ _ _ = return (Left "Stack argument is not a symbol or a lambda")

execJump :: [Env] -> [Variable] -> [Variable] -> [Insts] -> Int -> Int -> Int -> Maybe Bool -> IO (Either String Variable)
execJump _ _ _ _ _ _ (-1) _ = return (Left "Invalid number of instructions")
execJump _ _ _ insts idx _ num _ | num > 0 && num >= (length insts - idx) = return (Left "Invalid number of instructions")
execJump _ _ _ _ idx _ num _ | num < (idx + 1) * (-1) = return (Left "Invalid number of instructions")
execJump env args stack insts idx scopeIdx num Nothing = exec env args stack insts (idx + num + 1) scopeIdx
execJump env args ((Variable x _) : xs) insts idx scopeIdx num (Just b)
    | toBool x == b && num == length insts - idx = return $ Right (Variable Void scopeIdx)
    | toBool x == b = exec env args xs insts (idx + num + 1) scopeIdx
    | otherwise = exec env args xs insts (idx + 1) scopeIdx
execJump _ _ _ _ _ _ _ _ = return (Left "Stack is empty for a conditional jump")

execDefineEnv :: [Env] -> [Variable] -> [Variable] -> [Insts] -> Int -> Int -> String -> DefineEnvType -> Maybe Value -> IO (Either String Variable)
execDefineEnv _ _ [] _ _ _ _ _ Nothing = return (Left "Stack is empty for a DefineEnv from stack instruction")
execDefineEnv env args ((Variable val _) : xs) insts idx scopeIdx name x Nothing = execDefineEnv env args xs insts idx scopeIdx name x (Just val)
execDefineEnv env args stack insts idx scopeIdx name Define (Just val) =
    case findEnvValue name env of
        Just _ -> return (Left ("Environment " ++ name ++ " already exists"))
        Nothing -> exec (addEnvValue name (Variable val scopeIdx) env) args stack insts (idx + 1) scopeIdx
execDefineEnv env args stack insts idx scopeIdx name Redefine (Just val) =
    case findEnvValue name env of
        Just (Variable _ scope) -> exec (addEnvValue name (Variable val scope) env) args stack insts (idx + 1) scopeIdx
        Nothing -> return (Left ("Environment " ++ name ++ " does not exist"))
execDefineEnv env args stack insts idx scopeIdx name Override (Just val) =
    case findEnvValue name env of
        Just (Variable _ scope) -> exec (addEnvValue name (Variable val scope) env) args stack insts (idx + 1) scopeIdx
        Nothing -> exec (addEnvValue name (Variable val scopeIdx) env) args stack insts (idx + 1) scopeIdx

execOperation :: Value -> Value -> Operator -> Either String Value
execOperation (Integer _) (Integer 0) op | op == Div || op == Mod = Left "Cannot divide by 0"
execOperation (Float _) (Float 0) op | op == Div || op == Mod = Left "Cannot divide by 0"
execOperation (Float l) (Integer r) op = execOperation (Float l) (Float (int2Double r)) op
execOperation (Integer l) (Float r) op = execOperation (Float (int2Double l)) (Float r) op
execOperation (Integer l) (Integer r) op = case op of
    Add -> Right $ Integer $ l + r
    Sub -> Right $ Integer $ l - r
    Mul -> Right $ Integer $ l * r
    Div -> Right $ Integer $ div l r
    Mod -> Right $ Integer $ mod l r
    Pow -> Right $ Integer $ l ^ r
    Eq -> Right $ Bool $ l == r
    Neq -> Right $ Bool $ l /= r
    Less -> Right $ Bool $ l < r
    LessOrEqual -> Right $ Bool $ l <= r
    Greater -> Right $ Bool $ l > r
    GreaterOrEqual -> Right $ Bool $ l >= r
    _ -> Left ("Wrong data types in stack: " ++ show op ++ " with 2 integers")
execOperation (Float l) (Float r) op = case op of
    Add -> Right $ Float $ l + r
    Sub -> Right $ Float $ l - r
    Mul -> Right $ Float $ l * r
    Div -> Right $ Float $ l / r
    Mod -> Right $ Float $ mod' l r
    Pow -> Right $ Float $ l ** r
    Eq -> Right $ Bool $ l == r
    Neq -> Right $ Bool $ l /= r
    Less -> Right $ Bool $ l < r
    LessOrEqual -> Right $ Bool $ l <= r
    Greater -> Right $ Bool $ l > r
    GreaterOrEqual -> Right $ Bool $ l >= r
    _ -> Left ("Wrong data types in stack: " ++ show op ++ " with 2 floats")
execOperation (Bool l) (Bool r) op = case op of
    Eq -> Right $ Bool $ l == r
    Neq -> Right $ Bool $ l /= r
    And -> Right $ Bool $ l && r
    Or -> Right $ Bool $ l || r
    Xor -> Right $ Bool $ l /= r
    _ -> Left ("Wrong data types in stack: " ++ show op ++ " with 2 booleans")
execOperation (String str) (Integer nb) op = case op of
    Add -> Right $ String $ str ++ show nb
    Mul -> Right $ String $ concat $ replicate nb str
    _ -> Left ("Wrong data types in stack: " ++ show op ++ " with a string and an integer")
execOperation (String str) r op = case op of
    Add -> Right $ String $ str ++ show r
    Eq -> Right $ Bool $ str == show r
    Neq -> Right $ Bool $ str /= show r
    _ -> Left ("Wrong data types in stack: " ++ show op ++ " with a string as left operator")
execOperation l (String str) Add = Right $ String $ show l ++ str
execOperation _ _ op = Left ("Wrong stack variables for builtin " ++ show op)

findEnvValue :: String -> [Env] -> Maybe Variable
findEnvValue _ [] = Nothing
findEnvValue searchIdentifier (x : xs)
    | identifier x == searchIdentifier = Just $ value x
    | otherwise = findEnvValue searchIdentifier xs

addEnvValue :: String -> Variable -> [Env] -> [Env]
addEnvValue iden val (x : xs)
    | identifier x == iden = Env iden val : xs
    | otherwise = x : addEnvValue iden val xs
addEnvValue iden val [] = [Env iden val]

removeEnvValue :: String -> [Env] -> [Env]
removeEnvValue iden = filter (\x -> identifier x /= iden)

toBool :: Value -> Bool
toBool (Bool False) = False
toBool (Integer 0) = False
toBool (String "") = False
toBool Void = False
toBool _ = True
