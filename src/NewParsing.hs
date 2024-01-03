{-# LANGUAGE ViewPatterns #-}

module NewParsing (parseDreamberd) where

import NewTypes (AstNode (Number, Operator, String))

import Data.List (stripPrefix)
import Parsing (Parser(parse))

data DreamberdType = Number Int
                   | Bool Bool
                   | String String
                   | Function ([DreamberdType] -> DreamberdType)

detectType :: DreamberdType -> String
detectType (NewTypes.Number _) = "Number"
detectType (Bool _) = "Bool"
detectType (NewTypes.String _) = "String"
detectType (Function _) = "Function"

parseNumber :: String -> [AstNode] -> Either String (String, [AstNode])
parseNumber input ast = case span isDigit input of
    ("", _) -> Left "No number found"
    (numStr, rest) -> Right (rest, ast ++ [Number (read numStr)])

parseBool :: String -> [AstNode] -> Either String (String, [AstNode])
parseBool input ast
    | "true" `isPrefixOf` input  = Right (drop 4 input, ast ++ [Bool True])
    | "false" `isPrefixOf` input = Right (drop 5 input, ast ++ [Bool False])
    | otherwise                  = Left "No boolean found" 

parseString :: String -> [AstNode] -> Either String (String, [AstNode])
parseString input ast
    | head input /= '"' = Left "String does not start with a quote"
    | otherwise = case span (/= '"') (tail input) of
        (_, "") -> Left "No closing quote found"
        (str, rest) -> Right (tail rest, ast ++ [String str])


parseVar :: String -> [AstNode] -> Either String (String, [AstNode])
parseVar input ast = 
    parseNumber input ast <|> 
    parseBool input ast <|> 
    parseString input ast <|>
    Left "Unrecognized variable type"


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


-- TODO: add condition to stop when no more source code
-- donc le premier call a parseDreamberd sera avec tout le source code et une liste vide, il va se call lui même récursivement
-- et à la fin il va se stop quand y'a plus de source code (ou erreur, à gérer) et renvoyer l'AST complet
