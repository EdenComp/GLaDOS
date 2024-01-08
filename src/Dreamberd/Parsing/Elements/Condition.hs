{-# LANGUAGE ViewPatterns #-}

module Dreamberd.Parsing.Elements.Condition (parseConditionParts, parseConditionExpression) where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Dreamberd.Parsing.Elements.Operator (parseOperator)
import Dreamberd.Parsing.Utils (parseScope)
import Dreamberd.Parsing.Values (parseAnyValue)
import Dreamberd.Types (AstNode (Operator))

parseConditionExpression :: String -> Either String AstNode
parseConditionExpression input =
    case parseAnyValue (dropWhile isSpace input) of
        Right lhsValue ->
            let restInput = dropWhile isSpace (dropWhile (not . isSpace) input)
             in case parseOperator restInput of
                    Right (op, rest) ->
                        case parseAnyValue rest of
                            Right rhsValue -> Right (Operator op lhsValue rhsValue)
                            Left err -> Left err
                    Left err -> Left err
        Left err -> Left err

parseConditionParts :: String -> Either String (String, String, [(String, String)], Maybe String, String)
parseConditionParts str =
    if not ("(" `isPrefixOf` str && ')' `elem` str)
        then Left "If condition must start with '(' and end with ')'"
        else
            let (rawCondition, afterCondition) = break (== ')') str
                (dropWhile isSpace -> beforeCondition, dropWhile isSpace . drop 1 -> condition) = break (== '(') (dropWhile isSpace rawCondition)
             in if not (null beforeCondition) || null condition
                    then Left "Missing condition in if statement"
                    else
                        parseScope (drop 1 afterCondition) >>= \(ifBody, restAfterIf) ->
                            extractElifsAndElse restAfterIf >>= \(elifs, elsePart, restOfCode) ->
                                Right (condition, ifBody, elifs, elsePart, restOfCode)

extractElifsAndElse :: String -> Either String ([(String, String)], Maybe String, String)
extractElifsAndElse str
    | take 4 str == "elif" =
        parseConditionParts (dropWhile isSpace (drop 4 str)) >>= \(elifCondition, elifBody, elifs, elsePart, rest) ->
            if null elifCondition
                then Left "Missing condition in elif statement"
                else Right ((elifCondition, elifBody) : elifs, elsePart, rest)
    | take 4 str == "else" =
        parseScope (drop 4 str) >>= \(elseBody, restOfCode) -> Right ([], Just elseBody, restOfCode)
    | otherwise = Right ([], Nothing, str)
