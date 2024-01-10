{-# LANGUAGE ViewPatterns #-}

module Dreamberd.Parsing.Elements.Variable (parseVar) where

import Dreamberd.Parsing.Utils (extractValueAndRest, getVariableName, trimSpaces)
import Dreamberd.Parsing.Values (parseAnyValue, parseBool, parseFunctionCall, parseNumber, parseOperatorValue, parseString)
import Dreamberd.Types (AstNode (Call, Identifier, String))

parseVarValue :: Maybe String -> String -> Either String AstNode
parseVarValue (Just "int") value = case parseFunctionCall value of
    Right result -> Right result
    Left _ -> case parseOperatorValue value of
        Right result -> Right result
        Left _ -> case getVariableName value of
            Right name -> Right (Identifier name)
            Left _ -> parseNumber value >>= \result -> Right result
parseVarValue (Just "bool") value = case parseFunctionCall value of
    Right result -> Right result
    Left _ -> case parseOperatorValue value of
        Right result -> Right result
        Left _ -> case getVariableName value of
            Right name -> Right (Identifier name)
            Left _ -> parseBool value >>= \result -> Right result
parseVarValue (Just "str") value = case parseFunctionCall value of
    Right result -> Right result
    Left _ -> case parseOperatorValue value of
        Right result -> Right result
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
                strippedName = trimSpaces variableName
             in
                if length (words strippedName) > 1
                    then Left ("Invalid variable type '" ++ head (words strippedName) ++ "'")
                    else
                        getVariableName strippedName >>= \name ->
                            let
                                (value, restOfCode) = extractValueAndRest afterEqual
                             in
                                parseVarValue varType value >>= \node -> case varType of
                                    Just vt -> Right (restOfCode, ast ++ [Call "=" [String vt, Identifier name, node]])
                                    Nothing -> Right (restOfCode, ast ++ [Call "=" [Identifier name, node]])
