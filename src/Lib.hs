{-# LANGUAGE LambdaCase #-}

module Lib (glados, evaluateAndPrintResult) where

import Ast (evalAst)
import Parsing (parseLisp)
import ReadInput (getInput, processFile)
import SExpr (sexprsToAsts)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import Types (AstNode)

glados :: IO ()
glados =
    getArgs
        >>= \case
            [] -> getInput evaluateAndPrintResult
            [fileName] -> processFile evaluateAndPrintResult fileName
            _ -> putStrLn "Exit 84" >> exitWith (ExitFailure 84)

getEvaluatedResult :: String -> Maybe [AstNode]
getEvaluatedResult sourceCode = parseLisp sourceCode >>= sexprsToAsts >>= evalAst

printEvaluatedResult :: [AstNode] -> IO ()
printEvaluatedResult = mapM_ print

evaluateAndPrintResult :: String -> IO ()
evaluateAndPrintResult sourceCode =
    case getEvaluatedResult sourceCode of
        Just result -> printEvaluatedResult result
        Nothing -> exitWith (ExitFailure 84)
