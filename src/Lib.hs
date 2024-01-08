module Lib (glados) where

import Args (Command (..), commandsParser)
import Dreamberd.Lib (compileToAst, compileDreamberdCode, compileToVm, executeDreamberdBytecode, runDreamberdCode)
import Input (processFile, processInput)
import Lisp.Lib (evaluateAndPrintLispResult)

import Options.Applicative

glados :: IO ()
glados = execParser commandsParser >>= runGlados

runGlados :: Command -> IO ()
runGlados (Compile _ _ True True) = putStrLn "You cannot specify multiple format outputs"
runGlados (Compile file _ True False) = processFile compileToAst file
runGlados (Compile file _ False True) = processFile compileToVm file
runGlados (Compile file output False False) = processFile (`compileDreamberdCode` output) file
runGlados (Execute file) = processFile executeDreamberdBytecode file
runGlados (Run file) = processFile runDreamberdCode file
runGlados (Lisp (Just file)) = processFile evaluateAndPrintLispResult file
runGlados (Lisp Nothing) = processInput evaluateAndPrintLispResult
runGlados Version = putStrLn "GLaDOS v1.0.0"
