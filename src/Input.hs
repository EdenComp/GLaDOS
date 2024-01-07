module Input (
    processFile,
    processInput,
) where

import System.Exit (exitSuccess)
import System.IO (isEOF)

processFile :: (String -> IO ()) -> FilePath -> IO ()
processFile func fileName = readFile fileName >>= func

processInput :: (String -> IO ()) -> IO ()
processInput func = getUserInput func ""

getUserInput :: (String -> IO ()) -> String -> IO ()
getUserInput func linesSoFar = do
    eof <- isEOF
    if eof 
        then exitSuccess
    else do
        userInput <- getLine
        let updatedLines = linesSoFar ++ userInput
        func updatedLines
        getUserInput func updatedLines
