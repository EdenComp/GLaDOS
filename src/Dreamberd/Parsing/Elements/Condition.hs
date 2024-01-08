{-# LANGUAGE ViewPatterns #-}

module Dreamberd.Parsing.Elements.Condition (parseConditionParts) where

import Data.Char (isSpace)
import Dreamberd.Parsing.Utils (extractScopeAndRest)

parseConditionParts :: String -> Either String (String, String, [(String, String)], Maybe (String, String), String)
parseConditionParts str =
    let (beforeIf, afterIf) = break (== '{') str
        (filter (not . isSpace) -> condition, _) = break (== ')') beforeIf
        (ifBody, restAfterIf) = extractScopeAndRest (drop 1 afterIf) 1 []
     in if null condition
            then Left "Missing condition in if statement"
            else case extractElifsAndElse restAfterIf of
                Left err -> Left err
                Right (elifs, elsePart, restOfCode) -> Right (condition, ifBody, elifs, elsePart, restOfCode)

extractElifsAndElse :: String -> Either String ([(String, String)], Maybe (String, String), String)
extractElifsAndElse str
    | take 4 str == "elif" =
        case parseConditionParts (drop 4 str) of
            Left err -> Left err
            Right (elifCondition, elifBody, elifs, elsePart, rest) ->
                if null elifCondition
                    then Left "Missing condition in elif statement"
                    else Right ((elifCondition, elifBody) : elifs, elsePart, rest)
    | take 4 str == "else" =
        let (filter (not . isSpace) -> elseKeyword, restAfterElseKeyword) = break (== '{') (drop 4 str)
            (elseBody, restOfCode) = extractScopeAndRest (drop 1 restAfterElseKeyword) 1 []
         in if not (null elseKeyword)
                then Left "Unexpected condition in else statement"
                else Right ([], Just (elseKeyword, elseBody), restOfCode)
    | otherwise = Right ([], Nothing, str)
