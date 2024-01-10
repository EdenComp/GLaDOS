{-# LANGUAGE ViewPatterns #-}

module Dreamberd.Parsing.Elements.Loop (extractLoopParts) where

import Data.List (isPrefixOf)
import Dreamberd.Parsing.Utils (breakOnClosingParenthesis, parseScope, trimSpaces)

extractLoopParts :: String -> Either String (String, String, String)
extractLoopParts (trimSpaces -> str) =
    if not ("(" `isPrefixOf` str && ')' `elem` str)
        then Left "Loop condition must start with '(' and end with ')'"
        else
            breakOnClosingParenthesis str (-1) >>= \(rawCondition, afterCondition) ->
                let (trimSpaces -> beforeCondition, trimSpaces . drop 1 -> condition) = break (== '(') (trimSpaces rawCondition)
                 in if not (null beforeCondition) || null condition
                        then Left "Missing condition in loop statement"
                        else
                            parseScope afterCondition >>= \(loopBody, restOfCode) ->
                                Right (condition, loopBody, restOfCode)
