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
import Dreamberd.Parsing.Parser (parseDreamberd)
import Dreamberd.Types (File (..))
import Dreamberd.Vm (Insts (..), execVM)
import System.Exit (ExitCode (ExitFailure), exitWith)

compileDreamberdCode :: File String -> String -> IO ()
compileDreamberdCode file outputFile =
    case parseDreamberd file of
        Right ast -> case compileAst ast of
            Right insts -> writeFile outputFile (transpileIntoBytecode insts)
            Left err -> returnWithError err
        Left err -> returnWithError err

executeDreamberdBytecode :: File [Char] -> IO ()
executeDreamberdBytecode (File _ bytes) =
    case getFromBytecode bytes of
        Right insts -> executeDreamberdInsts insts
        Left err -> returnWithError err

executeDreamberdInsts :: [Insts] -> IO ()
executeDreamberdInsts insts = do
    ret <- execVM insts
    case ret of
        Right val -> print val
        Left err -> returnWithError err

runDreamberdCode :: File String -> IO ()
runDreamberdCode file =
    case parseDreamberd file of
        Right ast -> case compileAst ast of
            Right insts -> executeDreamberdInsts insts
            Left err -> returnWithError err
        Left err -> returnWithError err

compileToAst :: File String -> IO ()
compileToAst file =
    case parseDreamberd file of
        Right ast -> print ast
        Left err -> returnWithError err

compileToVm :: File String -> IO ()
compileToVm file =
    case parseDreamberd file of
        Right ast -> case compileAst ast of
            Right insts -> print insts
            Left err -> returnWithError err
        Left err -> returnWithError err

returnWithError :: String -> IO ()
returnWithError str = putStrLn str >> exitWith (ExitFailure 84)
