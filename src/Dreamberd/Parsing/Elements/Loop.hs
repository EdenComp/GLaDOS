{-# LANGUAGE ViewPatterns #-}

module Dreamberd.Parsing.Elements.Loop (extractLoopParts) where

import Data.Char (isSpace)
import Dreamberd.Parsing.Utils (parseScope)

extractLoopParts :: String -> Either String (String, String, String)
extractLoopParts str =
    let (beforeLoopScope, afterLoopScope) = break (== '{') str
        (filter (not . isSpace) -> loopTest, _) = break (== ')') beforeLoopScope
     in if null loopTest
            then Left "Missing test in loop"
            else parseScope afterLoopScope >>= \(loopBody, restOfCode) -> Right (loopTest, loopBody, restOfCode)
