module Dreamberd.Parsing.Utils (getVariableName, extractValueAndRest, parseScope, trimSpaces) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Text.Regex.Posix ((=~))

bannedVariables :: [String]
bannedVariables = ["if", "elif", "else", "true", "false", "return", "function", "int", "str", "bool"]

getVariableName :: String -> Either String String
getVariableName str = case str =~ "^[a-zA-Z_-]+$" :: (String, String, String) of
    (_, match, _) | null match -> Left "No variable name found"
    (_, match, _) | match `elem` bannedVariables -> Left $ "Variable name '" ++ match ++ "' is banned"
    (_, match, _) -> Right match

extractValueAndRest :: String -> (String, String)
extractValueAndRest = go False []
  where
    go _ acc [] = (reverse acc, [])
    go inQuotes acc (x : xs)
        | x == '"' = go (not inQuotes) (x : acc) xs
        | x == ';' && not inQuotes = (trimSpaces (reverse acc), xs)
        | otherwise = go inQuotes (x : acc) xs

extractScopeAndRest :: String -> Int -> String -> (String, String)
extractScopeAndRest [] _ body = (body, [])
extractScopeAndRest (x : xs) openBraces body
    | x == '{' = extractScopeAndRest xs (openBraces + 1) (body ++ [x])
    | x == '}' =
        if openBraces == 1
            then (body, trimSpaces xs)
            else extractScopeAndRest xs (openBraces - 1) (body ++ [x])
    | otherwise = extractScopeAndRest xs openBraces (body ++ [x])

checkStartsWithOpenBracket :: String -> Either String String
checkStartsWithOpenBracket [] = Left "Scope must start with open bracket but empty code found"
checkStartsWithOpenBracket (x : xs)
    | x == '{' = Right (x : xs)
    | otherwise = Left ("Code must start with an open bracket but starts with '" ++ [x] ++ "'")

parseScope :: String -> Either String (String, String)
parseScope code =
    checkStartsWithOpenBracket (trimSpaces code)
        >>= \validCode ->
            let (scope, rest) = extractScopeAndRest validCode 0 []
             in Right (drop 1 scope, rest)

trimSpaces :: String -> String
trimSpaces = dropWhileEnd isSpace . dropWhile isSpace
