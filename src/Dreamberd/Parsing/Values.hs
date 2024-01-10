{-# LANGUAGE ViewPatterns #-}

module Dreamberd.Parsing.Values (
    parseAnyValue,
    parseFunctionCall,
    parseNumber,
    parseBool,
    parseString,
    parseOperatorValue,
) where

import Data.Char (isDigit, isSpace)
import Data.Either (lefts, rights)
import Data.Foldable (find)
import Data.List (isInfixOf)
import Dreamberd.Parsing.Utils (getVariableName, splitOn, trimSpaces)
import Dreamberd.Types (AstNode (Boolean, Call, Identifier, Number, String))

parseBool :: String -> Either String AstNode
parseBool input
    | input == "true" = Right (Boolean True)
    | input == "false" = Right (Boolean False)
    | otherwise = Left "No boolean found"

parseNumber :: String -> Either String AstNode
parseNumber input
    | all isDigit input = Right (Number (read input))
    | otherwise = Left "No number found"

parseString :: String -> Either String AstNode
parseString input
    | head input /= '"' || last input /= '"' = Left "String does not start or end with a quote"
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
                let (params, remaining) = break (== ')') (drop 1 rest)
                    afterParams = drop 1 (trimSpaces (drop 1 remaining))
                    paramList = map parseAnyValue (words $ map (\c -> if c == ',' then ' ' else c) params)
                    errors = lefts paramList
                 in if not (null errors)
                        then Left (head errors)
                        else Right (afterParams, Call strippedName (rights paramList))

parseAnyValue :: String -> Either String AstNode
parseAnyValue "" = Left "No value found"
parseAnyValue input = case parseOperatorValue input of
    Right result -> Right result
    Left _ -> case parseString input of
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

parseOperatorValue :: String -> Either String AstNode
parseOperatorValue str =
    case findOperator str of
        Just op ->
            case splitOn op str of
                [trimSpaces -> lhs, trimSpaces -> rhs] ->
                    parseAnyValue lhs >>= \lhsNode ->
                        parseAnyValue rhs >>= \rhsNode ->
                            Right (Call op [lhsNode, rhsNode])
                _ -> Left "Invalid expression - expected two values around operator"
        _ -> Left "Invalid operator expression"

findOperator :: String -> Maybe String
findOperator str = find (`isInfixOf` str) ["<=", ">=", "==", "!=", "+", "-", "*", "/", "%", "<", ">"]
