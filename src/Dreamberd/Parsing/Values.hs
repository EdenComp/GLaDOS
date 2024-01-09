module Dreamberd.Parsing.Values (
    parseAnyValue,
    parseFunctionCall,
    parseNumber,
    parseBool,
    parseString,
) where

import Data.Char (isDigit, isSpace)
import Data.Either (lefts, rights)
import Data.List (isPrefixOf)
import Dreamberd.Parsing.Utils (getVariableName)
import Dreamberd.Types (AstNode (Boolean, Call, Identifier, Number, String))

parseBool :: String -> Either String AstNode
parseBool input
    | "true" `isPrefixOf` input = Right (Boolean True)
    | "false" `isPrefixOf` input = Right (Boolean False)
    | otherwise = Left "No boolean found"

parseNumber :: String -> Either String AstNode
parseNumber input = case span isDigit input of
    ("", _) -> Left "No number found"
    (numStr, _) -> Right (Number (read numStr))

parseString :: String -> Either String AstNode
parseString input
    | head input /= '"' = Left "String does not start with a quote"
    | otherwise = case span (/= '"') (tail input) of
        (_, "") -> Left "No closing quote found for String"
        (str, _) -> Right (String str)

parseFunctionCall :: String -> Either String (String, AstNode)
parseFunctionCall code =
    let (name, rest) = break (== '(') code
        strippedName = filter (not . isSpace) name
     in if null rest
            then Left "Invalid function call"
            else
                let paramsAndRest = init $ tail rest
                    (params, remaining) = span (/= ')') paramsAndRest
                    afterParams = dropWhile (\c -> c == ';' || isSpace c) $ tail remaining
                    paramList = map parseAnyValue (words $ map (\c -> if c == ',' then ' ' else c) params)
                    errors = lefts paramList
                 in if not (null errors)
                        then Left (head errors)
                        else Right (afterParams, Call strippedName (rights paramList))

parseAnyValue :: String -> Either String AstNode
parseAnyValue "" = Left "No value found"
parseAnyValue input = case parseString input of
    Right result -> Right result
    Left _ -> case parseNumber input of
        Right result -> Right result
        Left _ -> case parseBool input of
            Right result -> Right result
            Left _ -> case parseFunctionCall input of
                Right (_, result) -> Right result
                Left _ -> case getVariableName input of
                    Right name -> Right (Identifier name)
                    Left err -> Left ("Unable to parse value: " ++ err)
