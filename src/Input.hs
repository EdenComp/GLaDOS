module Input (
    processFile,
    processInput,
) where

import Dreamberd.Types (File (..))
import System.IO (isEOF)

processFile :: (File String -> IO ()) -> FilePath -> IO ()
processFile func fileName = readFile fileName >>= \sourceCode -> func (File fileName sourceCode)

processInput :: (String -> IO ()) -> IO ()
processInput func = getUserInput func ""

getUserInput :: (String -> IO ()) -> String -> IO ()
getUserInput func linesSoFar = do
    eof <- isEOF
    if eof
        then pure ()
        else do
            userInput <- getLine
            let updatedLines = linesSoFar ++ userInput
            func updatedLines
            getUserInput func updatedLines
