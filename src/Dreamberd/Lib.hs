{-# LANGUAGE LambdaCase #-}

module Dreamberd.Lib (
    compileDreamberdCode,
    compileToAst,
    compileToPostprocessedInsts,
    compileToPreprocessedAst,
    compileToVm,
    executeDreamberdBytecode,
    runDreamberdCode,
) where

import Dreamberd.Bytecode.Decode (getFromBytecode)
import Dreamberd.Bytecode.Encode (transpileIntoBytecode)
import Dreamberd.Bytecode.Pretty (prettyPrintInsts)
import Dreamberd.Compilation.Compile (compileAst)
import Dreamberd.Compilation.Preprocessing (executePreprocessing)
import Dreamberd.Compilation.Pretty (prettyPrintAST)
import Dreamberd.Parser (parseDreamberd)
import Dreamberd.Types (File (..))
import Dreamberd.Vm (Insts (..), Value (..), execVM)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Dreamberd.Compilation.Postprocessing (executePostprocessing)

compileDreamberdCode :: File String -> String -> IO ()
compileDreamberdCode (File filename sourcecode) outputFile =
    case parseDreamberd (File filename sourcecode) of
        Right ast ->
            executePreprocessing (File filename ast) >>= \case
                Right ast' -> case compileAst ast' of
                    Right insts -> writeFile outputFile $ transpileIntoBytecode $ executePostprocessing insts
                    Left err -> returnWithError err
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

runDreamberdCode :: File String -> IO ()
runDreamberdCode (File filename sourcecode) =
    case parseDreamberd (File filename sourcecode) of
        Right ast ->
            executePreprocessing (File filename ast) >>= \case
                Right ast' -> case compileAst ast' of
                    Right insts -> executeDreamberdInsts $ executePostprocessing insts
                    Left err -> returnWithError err
                Left err -> returnWithError err
        Left err -> returnWithError err

compileToAst :: File String -> IO ()
compileToAst file =
    case parseDreamberd file of
        Right ast -> putStr $ prettyPrintAST ast
        Left err -> returnWithError err

compileToPreprocessedAst :: File String -> IO ()
compileToPreprocessedAst (File filename sourcecode) =
    case parseDreamberd (File filename sourcecode) of
        Right ast ->
            executePreprocessing (File filename ast) >>= \case
                Right ast' -> putStr $ prettyPrintAST ast'
                Left err -> returnWithError err
        Left err -> returnWithError err

compileToVm :: File String -> IO ()
compileToVm (File filename sourcecode) =
    case parseDreamberd (File filename sourcecode) of
        Right ast ->
            executePreprocessing (File filename ast) >>= \case
                Right ast' -> case compileAst ast' of
                    Right insts -> putStr $ prettyPrintInsts insts
                    Left err -> returnWithError err
                Left err -> returnWithError err
        Left err -> returnWithError err

compileToPostprocessedInsts :: File String -> IO ()
compileToPostprocessedInsts (File filename sourcecode) =
    case parseDreamberd (File filename sourcecode) of
        Right ast ->
            executePreprocessing (File filename ast) >>= \case
                Right ast' -> case compileAst ast' of
                    Right insts -> putStr $ prettyPrintInsts $ executePostprocessing insts
                    Left err -> returnWithError err
                Left err -> returnWithError err
        Left err -> returnWithError err

returnWithError :: String -> IO ()
returnWithError str = putStrLn str >> exitWith (ExitFailure 84)
