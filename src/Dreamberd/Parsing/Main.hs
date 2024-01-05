{-# LANGUAGE ViewPatterns #-}

module Dreamberd.Parsing.Main (parseDreamberd) where

import Data.Char (isSpace)
import Data.List (stripPrefix)
import Dreamberd.Parsing.Elements.Function (extractFunctionParts, parseReturn)
import Dreamberd.Parsing.Elements.Variable (parseVar)
import Dreamberd.Parsing.Values (parseFunctionCall)
import Dreamberd.Types (AstNode (Function))

parseDreamberd :: String -> [AstNode] -> Either String [AstNode]
parseDreamberd "\n" ast = Right ast
parseDreamberd "" ast = Right ast
parseDreamberd sourceCode ast =
    case parseElement (dropWhile isSpace sourceCode) ast of
        Right (remainingCode, updatedAst) -> parseDreamberd remainingCode updatedAst
        Left err -> Left err

parseElement :: String -> [AstNode] -> Either String (String, [AstNode])
parseElement (stripPrefix "int" -> Just restCode) ast =
    case parseVar "int" restCode ast of
        Right (remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
        Left err -> Left err
parseElement (stripPrefix "bool" -> Just restCode) ast =
    case parseVar "bool" restCode ast of
        Right (remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
        Left err -> Left err
parseElement (stripPrefix "str" -> Just restCode) ast =
    case parseVar "str" restCode ast of
        Right (remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
        Left err -> Left err
parseElement (stripPrefix "function" -> Just restCode) ast =
    case parseFunction restCode ast of
        Right (remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
        Left err -> Left err
parseElement (stripPrefix "return" -> Just restCode) ast =
    case parseReturn restCode of
        Right (remainingCode, returnNode) -> Right (remainingCode, ast ++ [returnNode])
        Left err -> Left err
parseElement code ast = case parseFunctionCall code of
    Right (remainingCode, call) -> Right (remainingCode, ast ++ [call])
    Left _ -> Left "Unrecognized element"

parseFunction :: String -> [AstNode] -> Either String (String, [AstNode])
parseFunction code ast =
    let (name, params, body, restOfCode) = extractFunctionParts code
     in case parseDreamberd body [] of
            Right outputAst -> Right (restOfCode, ast ++ [Function name params outputAst])
            Left err -> Left err
