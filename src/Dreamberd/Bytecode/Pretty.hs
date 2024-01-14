module Dreamberd.Bytecode.Pretty (
    prettyPrintInsts,
) where

import Dreamberd.Vm (DefineEnvType (..), EnvValue (..), Insts (..), Value (..))

prettyPrintInsts :: [Insts] -> String
prettyPrintInsts insts = prettyPrintInstructions insts 0

prettyPrintInstructions :: [Insts] -> Int -> String
prettyPrintInstructions [] _ = ""
prettyPrintInstructions (Push val : xs) sp = replicate sp ' ' ++ "Push " ++ prettyPrintValue val ++ "\n" ++ prettyPrintInstructions xs sp
prettyPrintInstructions (PushArg arg : xs) sp = replicate sp ' ' ++ "PushArg " ++ show arg ++ "\n" ++ prettyPrintInstructions xs sp
prettyPrintInstructions (PushEnv env : xs) sp = replicate sp ' ' ++ "PushEnv " ++ env ++ "\n" ++ prettyPrintInstructions xs sp
prettyPrintInstructions (Call : xs) sp = replicate sp ' ' ++ "Call\n" ++ prettyPrintInstructions xs sp
prettyPrintInstructions (DefineEnv name def val : xs) sp = replicate sp ' ' ++ prettyPrintDefine name def val sp ++ "\n" ++ prettyPrintInstructions xs sp
prettyPrintInstructions (EraseEnv name : xs) sp = replicate sp ' ' ++ "EraseEnv " ++ name ++ "\n" ++ prettyPrintInstructions xs sp
prettyPrintInstructions (Jump val cond : xs) sp = replicate sp ' ' ++ prettyPrintJump val cond ++ "\n" ++ prettyPrintInstructions xs sp
prettyPrintInstructions (Ret : xs) sp = replicate sp ' ' ++ "Ret\n" ++ prettyPrintInstructions xs sp

prettyPrintValue :: Value -> String
prettyPrintValue (Integer nbr) = "Integer " ++ show nbr
prettyPrintValue (Float nbr) = "Float " ++ show nbr
prettyPrintValue (Bool b) = "Bool " ++ show b
prettyPrintValue (String str) = "String " ++ show str
prettyPrintValue (Symbol sym) = "Symbol " ++ show sym
prettyPrintValue Void = "Void"

prettyPrintDefine :: String -> DefineEnvType -> Maybe EnvValue -> Int -> String
prettyPrintDefine name Define Nothing _ = "DefineEnvFromStack " ++ name
prettyPrintDefine name Redefine Nothing _ = "RedefineEnvFromStack " ++ name
prettyPrintDefine name Override Nothing _ = "OverrideEnvFromStack " ++ name
prettyPrintDefine name Define (Just val) sp = "DefineEnv " ++ name ++ " " ++ prettyPrintEnvValue val sp
prettyPrintDefine name Redefine (Just val) sp = "RedefineEnv " ++ name ++ " " ++ prettyPrintEnvValue val sp
prettyPrintDefine name Override (Just val) sp = "OverrideEnv " ++ name ++ " " ++ prettyPrintEnvValue val sp

prettyPrintEnvValue :: EnvValue -> Int -> String
prettyPrintEnvValue (Function args insts) sp = "Function (" ++ show args ++ " arguments) (\n" ++ prettyPrintInstructions insts (sp + 2) ++ replicate sp ' ' ++ ")"
prettyPrintEnvValue (Variable val) _ = "Variable (" ++ prettyPrintValue val ++ ")"

prettyPrintJump :: Int -> Maybe Bool -> String
prettyPrintJump val Nothing = "Jump " ++ show val
prettyPrintJump val (Just True) = "JumpIfTrue " ++ show val
prettyPrintJump val (Just False) = "JumpIfFalse " ++ show val
