{-# LANGUAGE ViewPatterns #-}

module NewParsing (parseDreamberd) where

import NewTypes (AstNode (Number, Operator, String, Boolean))

import Data.List (stripPrefix, isPrefixOf)
import Parsing (Parser(parse))
import Control.Applicative
import Data.Char
import Data.Either
import Control.Applicative (Alternative (..))


data DreamberdType = Number Int
                   | Bool Bool
                   | String String
                   | Function ([DreamberdType] -> DreamberdType)


detectType :: DreamberdType -> String
detectType (NewParsing.Number _) = "Number"
detectType (NewParsing.String _) = "String"
detectType (Bool _) = "Bool"
detectType (Function _) = "Function"


parseNumber :: String -> [AstNode] -> Either String (String, [AstNode])
parseNumber input ast = case span isDigit input of
    ("", _) -> Left "No number found"
    (numStr, rest) -> Right (rest, ast ++ [NewTypes.Number (read numStr)])


parseBool :: String -> [AstNode] -> Either String (String, [AstNode])
parseBool input ast
    | "true" `isPrefixOf` input  = Right (drop 4 input, ast ++ [Boolean True])
    | "false" `isPrefixOf` input = Right (drop 5 input, ast ++ [Boolean False])
    | otherwise                  = Left "No boolean found" 

parseString :: String -> [AstNode] -> Either String (String, [AstNode])
parseString input ast
    | head input /= '"' = Left "String does not start with a quote"
    | otherwise = case span (/= '"') (tail input) of
        (_, "") -> Left "No closing quote found"
        (str, rest) -> Right (tail rest, ast ++ [NewTypes.String str])



parseVar :: String -> [AstNode] -> Either String (String, [AstNode])
parseVar input ast =
    case words input of
        ("int":varName:"=":rest) -> 
            let (value, remainingCode) = break (== ';') (unwords rest)
            in case parseNumber (value ++ ";") ast of
                Right (_, ast') -> Right (dropWhile (== ';') (tail remainingCode), ast' ++ [NewTypes.Number (read varName)])
                Left err -> Left err
        _ -> Left "Invalid variable declaration"



parseElement :: String -> [AstNode] -> Either String (String, [AstNode])
parseElement (stripPrefix "var" -> Just restCode) ast =
    case parseVar restCode ast of
        Right (remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
        Left err -> Left err
parseElement _ _ = Left "Unrecognized element"


parseDreamberd :: String -> [AstNode] -> Either String [AstNode]
parseDreamberd "" ast = Right ast
parseDreamberd sourceCode ast =
    case parseElement sourceCode ast of
        Right (remainingCode, updatedAst) -> parseDreamberd remainingCode updatedAst
        Left err -> Left err
