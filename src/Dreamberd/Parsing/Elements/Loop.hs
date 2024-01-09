{-# LANGUAGE ViewPatterns #-}

module Dreamberd.Parsing.Elements.Loop (extractLoopParts) where

import Data.List (isPrefixOf)
import Dreamberd.Parsing.Utils (parseScope, trimSpaces)

extractLoopParts :: String -> Either String (String, String, String)
extractLoopParts (trimSpaces -> str) =
    if not ("(" `isPrefixOf` str && ')' `elem` str)
        then Left "Loop condition must start with '(' and end with ')'"
        else
            let (beforeLoopScope, afterLoopScope) = break (== '{') str
                (trimSpaces . drop 1 -> loopTest, _) = break (== ')') beforeLoopScope
             in parseScope afterLoopScope >>= \(loopBody, restOfCode) -> Right (loopTest, loopBody, restOfCode)
