module GetInput
  ( getUserInput,
  )
where

import System.IO (hFlush, stdout)

colorRed :: String
colorRed = "\x1b[31m"

colorYellow :: String
colorYellow = "\x1b[33m"

colorReset :: String
colorReset = "\x1b[0m"

getUserInput :: IO ()
getUserInput = do
  putStr $ colorRed ++ "DreamBerd4-Interpret>>> " ++ colorReset
  hFlush stdout
  userInput <- getLine
  putStrLn $ "You entered: " ++ userInput
  if userInput /= "exit"
    then getUserInput
    else putStrLn $ colorYellow ++ "Exit DreamBerd4!" ++ colorReset