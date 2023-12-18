module ReadInput (
    processFile,
    getInput,
)
where

import GetFile (getFileContent)
import GetInput (getUserInput)

processFile :: (String -> IO ()) -> FilePath -> IO ()
processFile lispInterpreter fileName = getFileContent fileName >>= lispInterpreter

getInput :: (String -> IO ()) -> IO ()
getInput lispInterpreter =
        getUserInput lispInterpreter
