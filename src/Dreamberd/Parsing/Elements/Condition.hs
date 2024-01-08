{-# LANGUAGE ViewPatterns #-}

module Dreamberd.Parsing.Elements.Condition (parseConditionParts) where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Dreamberd.Parsing.Utils (parseScope)

parseConditionParts :: String -> Either String (String, String, [(String, String)], Maybe String, String)
parseConditionParts str =
    if not ("(" `isPrefixOf` str && ')' `elem` str)
        then Left "If condition must start with '(' and end with ')'"
        else
            let (rawCondition, afterCondition) = break (== ')') str
                (dropWhile isSpace -> beforeCondition, dropWhile isSpace . drop 1 -> condition) = break (== '(') (dropWhile isSpace rawCondition)
             in if not (null beforeCondition) || null condition
                    then Left "Missing condition in if statement"
                    else case parseScope (drop 1 afterCondition) of
                        Left err -> Left err
                        Right (ifBody, restAfterIf) ->
                            case extractElifsAndElse restAfterIf of
                                Left err -> Left err
                                Right (elifs, elsePart, restOfCode) -> Right (condition, ifBody, elifs, elsePart, restOfCode)

extractElifsAndElse :: String -> Either String ([(String, String)], Maybe String, String)
extractElifsAndElse str
    | take 4 str == "elif" =
        case parseConditionParts (dropWhile isSpace (drop 4 str)) of
            Left err -> Left err
            Right (elifCondition, elifBody, elifs, elsePart, rest) ->
                if null elifCondition
                    then Left "Missing condition in elif statement"
                    else Right ((elifCondition, elifBody) : elifs, elsePart, rest)
    | take 4 str == "else" =
        case parseScope (drop 4 str) of
            Left err -> Left err
            Right (elseBody, restOfCode) -> Right ([], Just elseBody, restOfCode)
    | otherwise = Right ([], Nothing, str)
