module Dreamberd.Lib (
    evaluateAndPrintDreamberdResult,
    executeDreamberdBytecode,
) where

import Dreamberd.Bytecode.Decode (getFromBytecode)
import Dreamberd.Parsing.Main (parseDreamberd)
import Dreamberd.Types (AstNode)
import Dreamberd.Vm (exec, Insts (..))
import System.Exit (ExitCode (ExitFailure), exitWith)

getDreamberdEvaluatedResult :: String -> Either String [AstNode]
getDreamberdEvaluatedResult sourceCode = parseDreamberd sourceCode []

evaluateAndPrintDreamberdResult :: String -> IO ()
evaluateAndPrintDreamberdResult sourceCode =
    case getDreamberdEvaluatedResult sourceCode of
        Right ast -> print ast
        Left err -> putStrLn err >> exitWith (ExitFailure 84)

executeDreamberdBytecode :: [Char] -> IO ()
executeDreamberdBytecode bytes =
    case getFromBytecode bytes of
        Right insts -> executeDreamberdInsts insts
        Left err -> putStrLn err >> exitWith (ExitFailure 84)

executeDreamberdInsts :: [Insts] -> IO ()
executeDreamberdInsts insts =
    case exec [] [] [] insts of
        Right res -> print res
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
