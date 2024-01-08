{-# LANGUAGE ViewPatterns #-}

module Dreamberd.Parsing.Elements.Variable (parseVar) where

import Data.Char (isSpace)
import Dreamberd.Parsing.Utils (extractValueAndRest, getVariableName)
import Dreamberd.Parsing.Values (parseAnyValue, parseBool, parseFunctionCall, parseNumber, parseString)
import Dreamberd.Types (AstNode (AssignVariable, Identifier, Operator))

parseVarValue :: Maybe String -> String -> Either String AstNode
parseVarValue (Just "int") value = case parseFunctionCall value of
    Right (_, result) -> Right result
    Left _ -> case getVariableName value of
        Right name -> Right (Identifier name)
        Left _ -> parseNumber value >>= \result -> Right result
parseVarValue (Just "bool") value = case parseFunctionCall value of
    Right (_, result) -> Right result
    Left _ -> case getVariableName value of
        Right name -> Right (Identifier name)
        Left _ -> parseBool value >>= \result -> Right result
parseVarValue (Just "str") value = case parseFunctionCall value of
    Right (_, result) -> Right result
    Left _ -> case getVariableName value of
        Right name -> Right (Identifier name)
        Left _ -> parseString value >>= \result -> Right result
parseVarValue Nothing value = parseAnyValue value
parseVarValue (Just varType) _ = Left ("Invalid variable value for type '" ++ varType ++ "'")

parseVar :: Maybe String -> String -> [AstNode] -> Either String (String, [AstNode])
parseVar varType code ast =
    if '=' `notElem` code
        then Left "No '=' found for variable assignment"
        else
            let
                (variableName, drop 1 -> afterEqual) = break (== '=') code
                strippedName = filter (not . isSpace) variableName
             in
                getVariableName strippedName >>= \name ->
                    let
                        (value, restOfCode) = extractValueAndRest afterEqual
                     in
                        parseVarValue varType value >>= \node -> case varType of
                            Just vt -> Right (restOfCode, ast ++ [AssignVariable vt name node])
                            Nothing -> Right (restOfCode, ast ++ [Operator "=" (Identifier name) node])
