module GetInput (
    getUserInput,
)
where

import System.IO (hFlush, isEOF, stdout)

colorRed :: String
colorRed = "\x1b[31m"

colorYellow :: String
colorYellow = "\x1b[33m"

colorReset :: String
colorReset = "\x1b[0m"

getUserInput :: IO ()
getUserInput =
    putStr (colorRed ++ "DreamBerd4-Interpreter>>> " ++ colorReset)
        >> hFlush stdout
        >> isEOF
        >>= \eof ->
            if eof
                then putStrLn (colorYellow ++ "Exit DreamBerd4!" ++ colorReset)
                else
                    getLine
                        >>= \userInput ->
                            putStrLn ("You entered: " ++ userInput)
                                >> getUserInput
