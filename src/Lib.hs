module Lib
  ( glados,
  )
where

import ReadInput (getInput, processFile)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

glados :: IO ()
glados =
  getArgs >>= \args ->
    case args of
      []          -> getInput
      [fileName]  -> processFile fileName
      _           -> putStrLn "Exit 84" >> exitWith (ExitFailure 84)