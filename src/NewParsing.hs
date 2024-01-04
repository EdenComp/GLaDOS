{-# LANGUAGE ViewPatterns #-}

module NewParsing (parseDreamberd) where

import NewTypes (AstNode (Number, Operator, String))

import Data.Char (isDigit, isSpace)
import Data.List (stripPrefix)

isNotSemiColon :: Char -> Bool
isNotSemiColon = (/= ';')

parseNumber :: String -> Either String AstNode
parseNumber input = case span isDigit input of
    ("", _) -> Left "No number found"
    (numStr, _) -> Right (Number (read numStr))

removeSemi :: String -> String
removeSemi xs = [x | x <- xs, x `notElem` ";"]

parseVar :: String -> [AstNode] -> Either String (String, [AstNode])
parseVar code ast = case parseNumber value of
    Right number -> Right (restOfCode, ast ++ [Operator "=" (String name) number])
    Left err -> Left err
  where
    scopedCode = words (takeWhile isNotSemiColon code)
    restOfCode = dropWhile isSpace (drop 1 (dropWhile isNotSemiColon code))
    name = head scopedCode
    value = removeSemi (last scopedCode)

parseElement :: String -> [AstNode] -> Either String (String, [AstNode])
parseElement (stripPrefix "var" -> Just restCode) ast =
    case parseVar restCode ast of
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
