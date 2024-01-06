{-# LANGUAGE ViewPatterns #-}

module Dreamberd.Parsing.Elements.Function (parseReturn, extractFunctionParts) where

import Data.Char (isSpace)
import Dreamberd.Parsing.Utils (extractValueAndRest)
import Dreamberd.Parsing.Values (parseAnyValue)
import Dreamberd.Types (AstNode (Return))

extractFunctionBodyAndRest :: String -> Int -> String -> (String, String)
extractFunctionBodyAndRest [] _ body = (body, [])
extractFunctionBodyAndRest (x : xs) openBraces body
    | x == '{' = extractFunctionBodyAndRest xs (openBraces + 1) (body ++ [x])
    | x == '}' =
        if openBraces - 1 == 0
            then (body, dropWhile isSpace xs)
            else extractFunctionBodyAndRest xs (openBraces - 1) (body ++ [x])
    | otherwise = extractFunctionBodyAndRest xs openBraces (body ++ [x])

extractFunctionParts :: String -> (String, [String], String, String)
extractFunctionParts str = (name, params, body, restOfCode)
  where
    (beforeBrace, afterBrace) = break (== '{') str
    (nameAndParams, _) = break (== ')') beforeBrace
    (filter (not . isSpace) -> name, parameters) = break (== '(') (dropWhile isSpace nameAndParams)
    params = words $ map (\c -> if c == ',' then ' ' else c) $ tail parameters
    (body, restOfCode) = extractFunctionBodyAndRest (drop 1 afterBrace) 1 []

parseReturn :: String -> Either String (String, AstNode)
parseReturn code =
    let (value, rest) = extractValueAndRest code
     in case parseAnyValue value of
            Right result -> Right (rest, Return result)
            Left err -> Left err
