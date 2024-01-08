{-# LANGUAGE ViewPatterns #-}

module Dreamberd.Parsing.Elements.Loop (extractLoopParts) where

import Data.Char (isSpace)
import Dreamberd.Parsing.Utils (extractScopeAndRest)

extractLoopParts :: String -> Either String (String, String, String)
extractLoopParts str =
    let (beforeLoopScope, afterLoopScope) = break (== '{') str
        (filter (not . isSpace) -> loopTest, _) = break (== ')') beforeLoopScope
        (loopBody, restOfCode) = extractScopeAndRest (drop 1 afterLoopScope) 1 []
     in if null loopTest
            then Left "Missing test in loop"
            else Right (loopTest, loopBody, restOfCode)
