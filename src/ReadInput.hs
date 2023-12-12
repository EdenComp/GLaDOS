module ReadInput (
    processFile,
    getInput,
)
where

import GetFile (getFileContent)
import GetInput (getUserInput)

processFile :: FilePath -> IO ()
processFile fileName =
    getFileContent fileName
        >>= \fileContent -> putStrLn ("Content of file " ++ fileName) >> putStrLn fileContent

getInput :: IO ()
getInput =
    putStrLn "Welcome to DreamBerd4-Interpreter!"
        >> getUserInput
