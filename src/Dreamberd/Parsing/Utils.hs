module Dreamberd.Parsing.Utils (getVariableName, extractValueAndRest, parseScope, trimSpaces, splitOn, breakOnClosingParenthesis) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import Text.Regex.Posix ((=~))

bannedVariables :: [String]
bannedVariables = ["if", "elif", "else", "true", "false", "return", "fn", "int", "str", "bool"]

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

splitOn :: String -> String -> [String]
splitOn [] _ = []
splitOn delim str = case breakOn delim str of
    (a, b)
        | null b -> [a]
        | otherwise -> a : splitOn delim (drop (length delim) b)

breakOn :: String -> String -> (String, String)
breakOn delim = func
  where
    func [] = ([], [])
    func str@(x : xs)
        | delim `isPrefixOf` str = span (/= head delim) str
        | otherwise = let (leftVal, rightVal) = func xs in (x : leftVal, rightVal)

breakOnClosingParenthesis :: String -> Int -> Either String (String, String)
breakOnClosingParenthesis = helper False
  where
    helper :: Bool -> String -> Int -> Either String (String, String)
    helper _ [] _ = Left "No closing parenthesis found"
    helper inString (c : cs) n
        | c == '"' && not inString = helper True cs n >>= \(inside, outside) -> Right (c : inside, outside)
        | c == '"' && inString = helper False cs n >>= \(inside, outside) -> Right (c : inside, outside)
        | c == '(' && not inString = helper inString cs (n + 1) >>= \(inside, outside) -> Right (c : inside, outside)
        | c == ')' && not inString && n == 0 = Right ([], cs)
        | c == ')' && not inString = helper inString cs (n - 1) >>= \(inside, outside) -> Right (c : inside, outside)
        | otherwise = helper inString cs n >>= \(inside, outside) -> Right (c : inside, outside)
