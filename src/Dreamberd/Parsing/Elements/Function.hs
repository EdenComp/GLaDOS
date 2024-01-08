{-# LANGUAGE ViewPatterns #-}

module Dreamberd.Parsing.Elements.Function (parseReturn, extractFunctionParts) where

import Data.Char (isSpace)
import Dreamberd.Parsing.Utils (extractScopeAndRest, extractValueAndRest)
import Dreamberd.Parsing.Values (parseAnyValue)
import Dreamberd.Types (AstNode (Return))

extractFunctionParts :: String -> (String, [String], String, String)
extractFunctionParts str = (name, params, body, restOfCode)
  where
    (beforeBrace, afterBrace) = break (== '{') str
    (nameAndParams, _) = break (== ')') beforeBrace
    (filter (not . isSpace) -> name, parameters) = break (== '(') (dropWhile isSpace nameAndParams)
    params = words $ map (\c -> if c == ',' then ' ' else c) $ tail parameters
    (body, restOfCode) = extractScopeAndRest (drop 1 afterBrace) 1 []

parseReturn :: String -> [AstNode] -> Either String (String, [AstNode])
parseReturn code ast =
    let (value, rest) = extractValueAndRest code
     in case parseAnyValue value of
            Right result -> Right (rest, ast ++ [Return result])
            Left err -> Left err
