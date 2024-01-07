module Lib (glados) where

import Args (Command (..), commandsParser)
import Dreamberd.Lib (compileDreamberdCode, executeDreamberdBytecode)
import Input (processFile, processInput)
import Lisp.Lib (evaluateAndPrintLispResult)

import Options.Applicative

glados :: IO ()
glados = execParser commandsParser >>= runGlados

runGlados :: Command -> IO ()
runGlados (Compile file output) = processFile (`compileDreamberdCode` output) file
runGlados (Execute file) = processFile executeDreamberdBytecode file
runGlados (Lisp (Just file)) = processFile evaluateAndPrintLispResult file
runGlados (Lisp Nothing) = processInput evaluateAndPrintLispResult
runGlados Version = putStrLn "GLaDOS v1.0.0"
