{-# LANGUAGE ViewPatterns #-}

module Dreamberd.Parsing.Elements.Condition (
    parseConditionParts,
    parseConditionExpression,
) where

import Dreamberd.Parsing.Utils (parseScope)
import Dreamberd.Parsing.Values (parseAnyValue)
import Dreamberd.Types (AstNode (Operator))

import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- | Parses a condition expression, e.g. if (x == 5) { ... } elif (x > 5) { ... } else { ... } etc...
parseConditionExpression :: String -> Either String AstNode
parseConditionExpression input =
    let trimmedInput = dropWhile isSpace input
        operators = ["<=", ">=", "==", "!=", "<", ">"]
        extractComponents [] acc = reverse acc
        extractComponents s acc =
            let (word, rest) = break isSpace s
                newAcc = if null word then acc else word : acc
             in extractComponents (dropWhile isSpace rest) newAcc
        components = extractComponents trimmedInput []
     in case components of
            [lhs, op, rhs] | op `elem` operators ->
                case (parseAnyValue lhs, parseAnyValue rhs) of
                    (Right lhsValue, Right rhsValue) -> Right (Operator op lhsValue rhsValue)
                    _ -> Left "Invalid expression"
            [single] -> parseAnyValue single
            _ -> Left "Invalid expression"

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
            Right ((elifCondition, elifBody) : elifs, elsePart, rest)
    | take 4 str == "else" =
        parseScope (drop 4 str) >>= \(elseBody, restOfCode) -> Right ([], Just elseBody, restOfCode)
    | otherwise = Right ([], Nothing, str)
