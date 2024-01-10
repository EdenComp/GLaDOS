{-# LANGUAGE ViewPatterns #-}

module Dreamberd.Parsing.Values (
    parseAnyValue,
    parseFunctionCall,
    parseNumber,
    parseBool,
    parseString,
    parseOperatorValue,
) where

import Data.Char (isDigit)
import Data.Either (lefts, rights)
import Data.Foldable (find)
import Data.List (isInfixOf)
import Dreamberd.Parsing.Utils (breakOnClosingParenthesis, getVariableName, splitOn, trimSpaces)
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

parseFunctionCall :: String -> Either String AstNode
parseFunctionCall code =
    if not ('(' `elem` code && ')' `elem` code)
        then Left "Function call must contain parenthesis for params"
        else
            let (trimSpaces -> strippedName, drop 1 -> paramsAndRest) = break (== '(') code
             in getVariableName strippedName >>= \name ->
                    breakOnClosingParenthesis paramsAndRest 0 >>= \(params, _) ->
                        let
                            paramList = map parseAnyValue (words $ map (\c -> if c == ',' then ' ' else c) params)
                            errors = lefts paramList
                         in
                            if not (null errors)
                                then Left (head errors)
                                else Right (Call name (rights paramList))

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
                    Right result -> Right result
                    Left _ -> case getVariableName input of
                        Right name -> Right (Identifier name)
                        Left err -> Left ("Unable to parse value: " ++ err)

parseOperatorValue :: String -> Either String AstNode
parseOperatorValue str =
    case find (`isInfixOf` str) ["<=", ">=", "==", "!=", "+", "-", "*", "/", "%", "<", ">"] of
        Just op ->
            case splitOn op str of
                [trimSpaces -> lhs, trimSpaces -> rhs] ->
                    parseAnyValue lhs >>= \lhsNode ->
                        parseAnyValue rhs >>= \rhsNode ->
                            Right (Call op [lhsNode, rhsNode])
                _ -> Left "Invalid expression - expected two values around operator"
        _ -> Left "Invalid operator expression"
