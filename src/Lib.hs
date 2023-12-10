module Lib
  ( glados,
  )
where

import ReadInput (getInput, processFile)
import System.Environment (getArgs)

glados :: IO ()
glados =
  getArgs >>= \args ->
    case args of
      [fileName] -> processFile fileName
      _           -> getInput
