module GetFile (
    getFileContent,
)
where

import Control.Exception (evaluate)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

getFileContent :: FilePath -> IO String
getFileContent fileName =
    openFile fileName ReadMode
        >>= \handle ->
            hGetContents handle
                >>= \fileContent ->
                    evaluate (length fileContent)
                        >> hClose handle
                        >> return fileContent
