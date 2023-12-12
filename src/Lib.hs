{-# LANGUAGE LambdaCase #-}

module Lib (glados) where

import ReadInput (getInput, processFile)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)

glados :: IO ()
glados =
    getArgs
        >>= \case
            [] -> getInput
            [fileName] -> processFile fileName
            _ -> putStrLn "Exit 84" >> exitWith (ExitFailure 84)
