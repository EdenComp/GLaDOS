module Dreamberd.Lib (
    compileDreamberdCode,
    executeDreamberdBytecode,
) where

import Dreamberd.Bytecode.Decode (getFromBytecode)
import Dreamberd.Bytecode.Encode (transpileIntoBytecode)
import Dreamberd.Compile (compileAst)
import Dreamberd.Parsing.Main (parseDreamberd)
import Dreamberd.Vm (Insts (..), exec)
import System.Exit (ExitCode (ExitFailure), exitWith)

compileDreamberdCode :: String -> IO ()
compileDreamberdCode sourceCode =
    case parseDreamberd sourceCode [] of
        Right ast -> case compileAst ast of
            Right insts -> writeFile "a.out" (transpileIntoBytecode insts)
            Left err -> putStrLn err >> exitWith (ExitFailure 84)
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
