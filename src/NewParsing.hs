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

parseString :: String -> Either String AstNode
parseString input
    | head input /= '"' = Left "String does not start with a quote"
    | otherwise = case span (/= '"') (tail input) of
        (_, "") -> Left "No closing quote found for String"
        (str, _) -> Right (String str)

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
    value = removeSemi (unwords (drop 2 scopedCode))
parseVar "bool" code ast = case parseBool value of
    Right bool -> Right (restOfCode, ast ++ [Operator "=" (String name) bool])
    Left err -> Left err
  where
    scopedCode = words (takeWhile isNotSemiColon code)
    restOfCode = dropWhile isSpace (drop 1 (dropWhile isNotSemiColon code))
    name = head scopedCode
    value = removeSemi (unwords (drop 2 scopedCode))
parseVar "str" code ast = case parseString value of
    Right str -> Right (restOfCode, ast ++ [Operator "=" (String name) str])
    Left err -> Left err
  where
    scopedCode = words (takeWhile isNotSemiColon code)
    restOfCode = dropWhile isSpace (drop 1 (dropWhile isNotSemiColon code))
    name = head scopedCode
    value = removeSemi (unwords (drop 2 scopedCode))
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
parseElement (stripPrefix "str" -> Just restCode) ast =
    case parseVar "str" restCode ast of
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
