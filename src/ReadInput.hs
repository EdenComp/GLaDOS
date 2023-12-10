module ReadInput
  ( processFile,
    getInput,
  )
where

import GetFile (getFileContent)
import GetInput (getUserInput)

processFile :: FilePath -> IO ()
processFile fileName = do
  fileContent <- getFileContent fileName
  putStrLn $ "Content of file " ++ fileName ++ " without comments:"
  putStrLn fileContent

getInput :: IO ()
getInput = do
  putStrLn "Welcome to DreamBerd4-Interpret!"
  getUserInput