{-# LANGUAGE LambdaCase #-}

module Lib (glados) where

import Dreamberd.Lib (compileDreamberdCode, executeDreamberdBytecode)
import Lisp.Lib (evaluateAndPrintLispResult)
import ReadInput (getInput, processFile)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitSuccess, exitWith)

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
            ["-h"] -> displayHelp >> exitSuccess
            ["--help"] -> displayHelp >> exitSuccess
            ["-v"] -> putStrLn "Glados v-1.0.0" >> exitSuccess
            ["--version"] -> putStrLn "Glados v-1.0.0" >> exitSuccess
            ["-c", fileName] -> processFile compileDreamberdCode fileName
            ["--compile", fileName] -> processFile compileDreamberdCode fileName
            ["-vm", fileName] -> processFile executeDreamberdBytecode fileName
            ["--virtual-machine", fileName] -> processFile executeDreamberdBytecode fileName
            ["-l", fileName] -> processFile evaluateAndPrintLispResult fileName
            ["--lisp", fileName] -> processFile evaluateAndPrintLispResult fileName
            ["-lr"] -> getInput evaluateAndPrintLispResult
            ["--lisp-repl"] -> getInput evaluateAndPrintLispResult
            arg -> wrongArgumentHandler arg

wrongArgumentHandler :: [String] -> IO ()
wrongArgumentHandler arg =
    putStr "Error: [\""
        >> putStr (unwords arg)
        >> putStrLn "\"] not Valid Flag."
        >> putStrLn "Check -h or --help for help."
        >> exitWith (ExitFailure 84)
