{-# LANGUAGE ViewPatterns #-}

module NewParsing (parseDreamberd) where

import NewTypes (AstNode (Boolean, Number, Operator, String))

import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf, stripPrefix)

isNotSemiColon :: Char -> Bool
isNotSemiColon = (/= ';')

parseBool :: String -> Either String AstNode
parseBool input
    | "true" `isPrefixOf` input = Right (Boolean True)
    | "false" `isPrefixOf` input = Right (Boolean False)
    | otherwise = Left "No boolean found"

parseNumber :: String -> Either String AstNode
parseNumber input = case span isDigit input of
    ("", _) -> Left "No number found"
    (numStr, _) -> Right (Number (read numStr))

removeSemi :: String -> String
removeSemi xs = [x | x <- xs, x `notElem` ";"]

parseVar :: String -> String -> [AstNode] -> Either String (String, [AstNode])
parseVar "int" code ast = case parseNumber value of
    Right number -> Right (restOfCode, ast ++ [Operator "=" (String name) number])
    Left err -> Left err
  where
    scopedCode = words (takeWhile isNotSemiColon code)
    restOfCode = dropWhile isSpace (drop 1 (dropWhile isNotSemiColon code))
    name = head scopedCode
    value = removeSemi (last scopedCode)
parseVar "bool" code ast = case parseBool value of
    Right bool -> Right (restOfCode, ast ++ [Operator "=" (String name) bool])
    Left err -> Left err
  where
    scopedCode = words (takeWhile isNotSemiColon code)
    restOfCode = dropWhile isSpace (drop 1 (dropWhile isNotSemiColon code))
    name = head scopedCode
    value = removeSemi (last scopedCode)
parseVar _ _ _ = Left "Unrecognized variable type"

parseElement :: String -> [AstNode] -> Either String (String, [AstNode])
parseElement (stripPrefix "int" -> Just restCode) ast =
    case parseVar "int" restCode ast of
        Right (remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
        Left err -> Left err
parseElement (stripPrefix "bool" -> Just restCode) ast =
    case parseVar "bool" restCode ast of
        Right (remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
        Left err -> Left err
parseElement _ _ = Left "Unrecognized element"

parseDreamberd :: String -> [AstNode] -> Either String [AstNode]
parseDreamberd "\n" ast = Right ast
parseDreamberd "" ast = Right ast
parseDreamberd sourceCode ast =
    case parseElement sourceCode ast of
        Right (remainingCode, updatedAst) -> parseDreamberd remainingCode updatedAst
        Left err -> Left err
