module Dreamberd.Parsing.Elements.Function (parseReturn, extractFunctionParts) where

import Data.Char (isSpace)
import Dreamberd.Parsing.Utils (extractValueAndRest, getVariableName, parseScope)
import Dreamberd.Parsing.Values (parseAnyValue)
import Dreamberd.Types (AstNode (Return))

extractFunctionParts :: String -> Either String (String, [String], String, String)
extractFunctionParts str =
    if not ('(' `elem` str && ')' `elem` str)
        then Left "No parenthesis found for function params"
        else
            let (nameAndParams, restAfterParams) = break (== ')') str
                (_, parameters) = break (== '(') (dropWhile isSpace nameAndParams)
                params = words $ map (\c -> if c == ',' then ' ' else c) $ tail parameters
             in case getVariableName (dropWhile isSpace nameAndParams) of
                    Left err -> Left err
                    Right name ->
                        case parseScope (drop 1 restAfterParams) of
                            Left err -> Left err
                            Right (body, restOfCode) -> Right (name, params, body, restOfCode)

parseReturn :: String -> [AstNode] -> Either String (String, [AstNode])
parseReturn code ast =
    let (value, rest) = extractValueAndRest code
     in case parseAnyValue value of
            Right result -> Right (rest, ast ++ [Return result])
            Left err -> Left err
