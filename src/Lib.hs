{-# LANGUAGE LambdaCase #-}

module Lib (glados, evaluateAndPrintResult) where

import Ast (evalAst)
import NewParsing (parseDreamberd)
import NewTypes
import Parsing (parseLisp)
import ReadInput (getInput, processFile)
import SExpr (sexprsToAsts)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import Types

displayHelp :: IO ()
displayHelp =
    putStrLn
        "\
        \Usage: ./glados [OPTIONS] [FILE]\n\n\
        \Options:\n\
        \  -h,  --help\t\t\tDisplay this help message\n\
        \  -v,  --version\t\tDisplay version information\n\
        \  -l,  --lisp FILE\t\tLoad and execute the content of FILE that contains Lisp code\n\
        \  -c,  --compile FILE\t\tCompile FILE into an executable\n\
        \  -vm, --virtual-machine FILE\tExecute the compiled FILE\n\
        \  -lr, --lisp-repl\t\tLaunch a Lisp REPL\n\
        \"

glados :: IO ()
glados =
    getArgs
        >>= \case
            [] -> putStrLn "No arguments, -h or --help for help" >> exitWith (ExitFailure 84)
            ["-vm", fileName] -> putStr "Launch VM " >> putStr fileName >> putStrLn "..." >> exitSuccess
            ["--virtual-machine", fileName] -> putStr "Launch VM " >> putStr fileName >> putStrLn "..." >> exitSuccess
            ["-h"] -> displayHelp >> exitSuccess
            ["--help"] -> displayHelp >> exitSuccess
            ["-v"] -> putStrLn "Glados v-1.0.0" >> exitSuccess
            ["--version"] -> putStrLn "Glados v-1.0.0" >> exitSuccess
            ["-c", fileName] -> processFile evaluateAndPrintNewResult fileName
            ["--compile", fileName] -> putStr "Launch Compile " >> putStr fileName >> putStrLn "..." >> exitSuccess
            ["-l", fileName] -> processFile evaluateAndPrintResult fileName
            ["--lisp", fileName] -> processFile evaluateAndPrintResult fileName
            ["-lr"] -> getInput evaluateAndPrintResult
            ["--lisp-repl"] -> getInput evaluateAndPrintResult
            arg -> wrongArgumentHandler arg

wrongArgumentHandler :: [String] -> IO ()
wrongArgumentHandler arg =
    putStr "Error: [\""
        >> putStr (unwords arg)
        >> putStrLn "\"] not Valid Flag."
        >> putStrLn "Check -h or --help for help."
        >> exitWith (ExitFailure 84)

getEvaluatedResult :: String -> Maybe [Types.AstNode]
getEvaluatedResult sourceCode = parseLisp sourceCode >>= sexprsToAsts >>= evalAst

getNewEvaluatedResult :: String -> Either String [NewTypes.AstNode]
getNewEvaluatedResult sourceCode = parseDreamberd sourceCode []

printEvaluatedResult :: [Types.AstNode] -> IO ()
printEvaluatedResult = mapM_ print

evaluateAndPrintResult :: String -> IO ()
evaluateAndPrintResult sourceCode =
    case getEvaluatedResult sourceCode of
        Just result -> printEvaluatedResult result
        Nothing -> exitWith (ExitFailure 84)

evaluateAndPrintNewResult :: String -> IO ()
evaluateAndPrintNewResult sourceCode =
    case getNewEvaluatedResult sourceCode of
        Right ast -> print ast
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
