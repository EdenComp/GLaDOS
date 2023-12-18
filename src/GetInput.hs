module GetInput (
    getUserInput,
)
where

import System.Exit (exitSuccess)
import System.IO (isEOF)

getUserInput :: (String -> IO ()) -> IO ()
getUserInput lispInterpreter = getUserInput' lispInterpreter ""

getUserInput' :: (String -> IO ()) -> String -> IO ()
getUserInput' lispInterpreter linesSoFar = do
    eof <- isEOF
    if eof
        then exitSuccess
        else do
            userInput <- getLine
            let updatedLines = linesSoFar ++ userInput
            lispInterpreter updatedLines
            getUserInput' lispInterpreter updatedLines
