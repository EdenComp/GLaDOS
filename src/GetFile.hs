module GetFile
  ( getFileContent,
  )
where

import Control.Exception (evaluate)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

getFileContent :: FilePath -> IO String
getFileContent fileName = do
  handle <- openFile fileName ReadMode
  fileContent <- hGetContents handle
  _ <- evaluate (length fileContent)
  hClose handle
  return fileContent