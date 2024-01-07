module ReadInput (
    processFile,
    getInput,
)
where

import GetInput (getUserInput)

processFile :: (String -> IO ()) -> FilePath -> IO ()
processFile lispInterpreter fileName = readFile fileName >>= lispInterpreter

getInput :: (String -> IO ()) -> IO ()
getInput = getUserInput
