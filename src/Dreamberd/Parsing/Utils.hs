module Dreamberd.Parsing.Utils (getVariableName, extractValueAndRest) where

import Data.Char (isSpace)
import Text.Regex.Posix ((=~))

bannedVariables :: [String]
bannedVariables = ["if", "elif", "else", "true", "false", "return", "function", "int", "str", "bool"]

getVariableName :: String -> Either String String
getVariableName str = case str =~ "^[a-zA-Z_-]+" :: (String, String, String) of
    (_, match, _) | null match -> Left "No variable name found"
    (_, match, _) | match `elem` bannedVariables -> Left $ "Variable name " ++ show match ++ " is banned"
    (_, match, _) -> Right match

extractValueAndRest :: String -> (String, String)
extractValueAndRest = go False []
  where
    go _ acc [] = (reverse acc, [])
    go inQuotes acc (x : xs)
        | x == '"' = go (not inQuotes) (x : acc) xs
        | x == ';' && not inQuotes = (dropWhile isSpace (reverse acc), xs)
        | otherwise = go inQuotes (x : acc) xs
