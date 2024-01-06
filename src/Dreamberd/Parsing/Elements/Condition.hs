{-# LANGUAGE ViewPatterns #-}

module Dreamberd.Parsing.Elements.Condition (parseConditionParts) where

import Data.Char (isSpace)
import Dreamberd.Parsing.Elements.Function (extractFunctionBodyAndRest)

parseConditionParts :: String -> (String, String, [(String, String)], Maybe (String, String), String)
parseConditionParts str = (condition, ifBody, elifs, elsePart, restOfCode)
  where
    (beforeIf, afterIf) = break (== '{') str
    (filter (not . isSpace) -> condition, _) = break (== ')') beforeIf
    (ifBody, restAfterIf) = extractFunctionBodyAndRest (drop 1 afterIf) 1 []
    (elifs, elsePart, restOfCode) = extractElifsAndElse restAfterIf

extractElifsAndElse :: String -> ([(String, String)], Maybe (String, String), String)
extractElifsAndElse str
    | take 4 str == "elif" =
        let (elifCondition, elifBody, elifs, elsePart, rest) = parseConditionParts (drop 4 str)
         in ((elifCondition, elifBody) : elifs, elsePart, rest)
    | take 4 str == "else" =
        let (elseKeyword, restAfterElseKeyword) = break (== '{') (drop 4 str)
         in let (elseBody, restOfCode) = extractFunctionBodyAndRest (drop 1 restAfterElseKeyword) 1 []
             in ([], Just (elseKeyword, elseBody), restOfCode)
    | otherwise = ([], Nothing, str)
