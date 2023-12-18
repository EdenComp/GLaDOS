module GetInput (
    getUserInput,
)
where

import System.IO (isEOF)

getUserInput :: (String -> IO ()) -> IO ()
getUserInput lispInterpreter =
    isEOF
    >>= \eof ->
        if eof
            then putStrLn "Exit"
            else
                getLine
                >>= \userInput -> lispInterpreter userInput >> getUserInput lispInterpreter
