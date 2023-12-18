module GetInput (
    getUserInput,
)
where

import System.IO (isEOF)
import System.Exit (exitWith, ExitCode (..))

getUserInput :: (String -> IO ()) -> IO ()
getUserInput lispInterpreter = getUserInput' lispInterpreter ""

getUserInput' :: (String -> IO ()) -> String -> IO ()
getUserInput' lispInterpreter linesSoFar = do
    eof <- isEOF
    if eof
        then exitWith ExitSuccess
        else do
            userInput <- getLine
            let updatedLines = linesSoFar ++ userInput
            lispInterpreter updatedLines
            getUserInput' lispInterpreter updatedLines

