module Dreamberd.Lib (
    compileDreamberdCode,
    compileToAst,
    compileToVm,
    executeDreamberdBytecode,
    runDreamberdCode,
) where

import Dreamberd.Bytecode.Decode (getFromBytecode)
import Dreamberd.Bytecode.Encode (transpileIntoBytecode)
import Dreamberd.Compile (compileAst)
import Dreamberd.Parsing.Main (parseDreamberd)
import Dreamberd.Vm (Insts (..), Value (..), exec)
import System.Exit (ExitCode (ExitFailure), exitWith)

compileDreamberdCode :: String -> String -> IO ()
compileDreamberdCode sourceCode outputFile =
    case parseDreamberd sourceCode [] of
        Right ast -> case compileAst ast of
            Right insts -> writeFile outputFile (transpileIntoBytecode insts)
            Left err -> returnWithError err
        Left err -> returnWithError err

executeDreamberdBytecode :: [Char] -> IO ()
executeDreamberdBytecode bytes =
    case getFromBytecode bytes of
        Right insts -> executeDreamberdInsts insts
        Left err -> returnWithError err

executeDreamberdInsts :: [Insts] -> IO ()
executeDreamberdInsts insts = do
    ret <- exec [] [] [] insts
    case ret of
        Right val -> print val
        Left err -> returnWithError err

runDreamberdCode :: String -> IO ()
runDreamberdCode sourceCode =
    case parseDreamberd sourceCode [] of
        Right ast -> case compileAst ast of
            Right insts -> executeDreamberdInsts insts
            Left err -> returnWithError err
        Left err -> returnWithError err

compileToAst :: String -> IO ()
compileToAst sourceCode =
    case parseDreamberd sourceCode [] of
        Right ast -> print ast
        Left err -> returnWithError err

compileToVm :: String -> IO ()
compileToVm sourceCode =
    case parseDreamberd sourceCode [] of
        Right ast -> case compileAst ast of
            Right insts -> print insts
            Left err -> returnWithError err
        Left err -> returnWithError err

returnWithError :: String -> IO ()
returnWithError str = putStrLn str >> exitWith (ExitFailure 84)
