{-# LANGUAGE ViewPatterns #-}

module Dreamberd.Parsing.Main (parseDreamberd, parseFunction, parseCondition) where

import Data.Char (isSpace)
import Data.List (isPrefixOf, stripPrefix)
import Dreamberd.Parsing.Elements.Condition (parseConditionParts)
import Dreamberd.Parsing.Elements.Function (extractFunctionParts, parseReturn)
import Dreamberd.Parsing.Elements.Variable (parseVar)
import Dreamberd.Parsing.Values (parseFunctionCall)
import Dreamberd.Types (AstNode (Boolean, Function, If))
import Dreamberd.Parsing.Elements.Condition (parseConditionExpression)

parseDreamberd :: String -> [AstNode] -> Either String [AstNode]
parseDreamberd sourceCode ast
    | all isSpace sourceCode = Right ast
    | otherwise =
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
parseElement code ast
    | "if" `isPrefixOf` code =
        case parseCondition code ast of
            Right (remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
            Left err -> Left err
    | otherwise =
        case parseFunctionCall code of
            Right (remainingCode, call) -> Right (remainingCode, ast ++ [call])
            Left _ -> Left "Unrecognized element"

parseFunction :: String -> [AstNode] -> Either String (String, [AstNode])
parseFunction code ast =
    let (name, params, body, restOfCode) = extractFunctionParts code
     in case parseDreamberd body [] of
            Right outputAst -> Right (restOfCode, ast ++ [Function name params outputAst])
            Left err -> Left err

parseCondition :: String -> [AstNode] -> Either String (String, [AstNode])
parseCondition str ast =
    case parseConditionParts str of
        Left err -> Left err
        Right (condition, ifBody, elifs, elsePart, restOfCode) ->
            case buildConditionNodes condition ifBody elifs elsePart of
                Right ifNodes -> Right (restOfCode, ast ++ ifNodes)
                Left err -> Left err

buildConditionNodes :: String -> String -> [(String, String)] -> Maybe (String, String) -> Either String [AstNode]
buildConditionNodes cond ifBody [] Nothing = do
    ifCondAst <- parseConditionExpression cond
    ifBodyAst <- parseDreamberd ifBody []    
    return [If ifCondAst ifBodyAst []]
buildConditionNodes cond ifBody ((elifCondition, elifBody) : elifs) elsePart = do
    ifCondAst <- parseConditionExpression cond
    ifBodyAst <- parseDreamberd ifBody []
    elifCondAst <- parseConditionExpression elifCondition
    elifBodyAst <- parseDreamberd elifBody []
    elifNodes <- buildConditionNodes elifCondition elifBody elifs elsePart
    return [If ifCondAst ifBodyAst (elifNodes ++ [If elifCondAst elifBodyAst []])]
buildConditionNodes cond ifBody [] (Just (elseCond, elseBody)) = do
    ifCondAst <- parseConditionExpression cond
    ifBodyAst <- parseDreamberd ifBody []
    elseCondAst <- parseConditionExpression elseCond
    elseBodyAst <- parseDreamberd elseBody []
    return [If ifCondAst ifBodyAst [], If elseCondAst elseBodyAst []]

