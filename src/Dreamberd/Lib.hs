{-# LANGUAGE LambdaCase #-}

module Dreamberd.Lib (
    compileDreamberdCode,
    compileToAst,
    compileToPreprocessedAst,
    compileToVm,
    executeDreamberdBytecode,
    runDreamberdCode,
) where

import Dreamberd.Bytecode.Decode (getFromBytecode)
import Dreamberd.Bytecode.Encode (transpileIntoBytecode)
import Dreamberd.Compile (compileAst)
import Dreamberd.Parser (parseDreamberd)
import Dreamberd.Types (File (..))
import Dreamberd.Vm (Insts (..), Value (..), execVM)
import System.Exit (ExitCode (ExitFailure), exitWith)

{--
import Dreamberd.Compilation.Compile (compileAst)
import Dreamberd.Compilation.Preprocessing (executePreprocessing)
import Dreamberd.Parsing.Main (parseDreamberd)
import Dreamberd.Vm (Insts (..), execVM)
import System.Exit (ExitCode (ExitFailure), exitWith)

compileDreamberdCode :: String -> String -> IO ()
compileDreamberdCode sourceCode outputFile =
    case parseDreamberd sourceCode [] of
        Right ast ->
            executePreprocessing ast >>= \case
                Right ast' -> case compileAst ast' of
                    Right insts -> writeFile outputFile (transpileIntoBytecode insts)
                    Left err -> returnWithError err
                Left err -> returnWithError err
--}

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
        Right Void -> return ()
        Right (Integer nb) -> exitWith (ExitFailure nb)
        Right (Float nb) -> exitWith (ExitFailure (floor nb))
        Right (Bool b) -> exitWith (ExitFailure $ fromEnum b)
        Right val -> putStrLn ("Warning: main scope returned a non-numerical value: " ++ show val)
        Left err -> returnWithError err

{--
runDreamberdCode :: String -> IO ()
runDreamberdCode sourceCode =
    case parseDreamberd sourceCode [] of
        Right ast ->
            executePreprocessing ast >>= \case
                Right ast' -> case compileAst ast' of
                    Right insts -> executeDreamberdInsts insts
                    Left err -> returnWithError err
                Left err -> returnWithError err
--}

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

compileToPreprocessedAst :: String -> IO ()
compileToPreprocessedAst sourceCode =
    case parseDreamberd sourceCode [] of
        Right ast ->
            executePreprocessing ast >>= \case
                Right ast' -> print ast'
                Left err -> returnWithError err
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
