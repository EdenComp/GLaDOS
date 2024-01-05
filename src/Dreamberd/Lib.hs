module Dreamberd.Lib (
    evaluateAndPrintDreamberdResult,
) where

import Dreamberd.Parsing.Main (parseDreamberd)
import Dreamberd.Types (AstNode)
import System.Exit (ExitCode (ExitFailure), exitWith)

getDreamberdEvaluatedResult :: String -> Either String [AstNode]
getDreamberdEvaluatedResult sourceCode = parseDreamberd sourceCode []

evaluateAndPrintDreamberdResult :: String -> IO ()
evaluateAndPrintDreamberdResult sourceCode =
    case getDreamberdEvaluatedResult sourceCode of
        Right ast -> print ast
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
