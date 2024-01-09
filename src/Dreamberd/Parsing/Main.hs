{-# LANGUAGE ViewPatterns #-}

module Dreamberd.Parsing.Main (parseDreamberd, parseFunction, parseCondition) where

import Data.Char (isSpace)
import Data.List (stripPrefix)
import Dreamberd.Parsing.Elements.Condition (
    parseConditionExpression,
    parseConditionParts,
 )
import Dreamberd.Parsing.Elements.Function (extractFunctionParts, parseReturn)
import Dreamberd.Parsing.Elements.Loop (extractLoopParts)
import Dreamberd.Parsing.Elements.Variable (parseVar)
import Dreamberd.Parsing.Values (parseFunctionCall)
import Dreamberd.Types (AstNode (Boolean, Function, If, Loop))

parseDreamberd :: String -> [AstNode] -> Either String [AstNode]
parseDreamberd sourceCode ast
    | all isSpace sourceCode = Right ast
    | otherwise =
        parseElement (dropWhile isSpace sourceCode) ast >>= uncurry parseDreamberd

parseElement :: String -> [AstNode] -> Either String (String, [AstNode])
parseElement (stripPrefix "int" -> Just restCode) ast =
    parseVar (Just "int") restCode ast >>= \(remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
parseElement (stripPrefix "bool" -> Just restCode) ast =
    parseVar (Just "bool") restCode ast >>= \(remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
parseElement (stripPrefix "str" -> Just restCode) ast =
    parseVar (Just "str") restCode ast >>= \(remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
parseElement (stripPrefix "function" -> Just restCode) ast =
    parseFunction restCode ast >>= \(remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
parseElement (stripPrefix "return" -> Just restCode) ast =
    parseReturn restCode ast >>= \(remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
parseElement (stripPrefix "while" -> Just restCode) ast =
    parseLoop "while" restCode ast >>= \(remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
parseElement (stripPrefix "if" -> Just restCode) ast =
    parseCondition restCode ast >>= \(remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
parseElement code ast =
    case parseFunctionCall code of
        Right (remainingCode, call) -> Right (remainingCode, ast ++ [call])
        Left _ -> case parseVar Nothing code ast of
            Right (remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
            Left _ -> Left "Unrecognized element"

parseFunction :: String -> [AstNode] -> Either String (String, [AstNode])
parseFunction code ast =
    extractFunctionParts code >>= \(name, params, body, restOfCode) ->
        parseDreamberd body [] >>= \outputAst -> Right (restOfCode, ast ++ [Function name params outputAst])

parseLoop :: String -> String -> [AstNode] -> Either String (String, [AstNode])
parseLoop "while" code ast =
    extractLoopParts code >>= \(_loopTest, body, restOfCode) ->
        parseDreamberd body [] >>= \outputAst -> Right (restOfCode, ast ++ [Loop (Boolean True) outputAst Nothing Nothing])
parseLoop _ _ _ = Left "Unrecognized loop type"

parseCondition :: String -> [AstNode] -> Either String (String, [AstNode])
parseCondition str ast =
    parseConditionParts (dropWhile isSpace str)
        >>= \(condition, ifBody, elifs, elsePart, restOfCode) ->
            buildConditionNodes condition ifBody elifs elsePart >>= \ifNodes -> Right (restOfCode, ast ++ ifNodes)

buildConditionNodes :: String -> String -> [(String, String)] -> Maybe String -> Either String [AstNode]
buildConditionNodes cond ifBody [] Nothing =
    parseConditionExpression cond
        >>= \ifCondAst ->
            parseDreamberd ifBody []
                >>= \ifBodyAst -> return [If ifCondAst ifBodyAst []]
buildConditionNodes cond ifBody ((elifCondition, elifBody) : elifs) elsePart =
    parseConditionExpression cond
        >>= \ifCondAst ->
            parseDreamberd ifBody []
                >>= \ifBodyAst ->
                    buildConditionNodes elifCondition elifBody elifs elsePart
                        >>= \elifNodes -> return [If ifCondAst ifBodyAst elifNodes]
buildConditionNodes cond ifBody [] (Just elseBody) =
    parseConditionExpression cond
        >>= \ifCondAst ->
            parseDreamberd ifBody []
                >>= \ifBodyAst ->
                    parseDreamberd elseBody []
                        >>= \elseBodyAst -> return [If ifCondAst ifBodyAst elseBodyAst]
