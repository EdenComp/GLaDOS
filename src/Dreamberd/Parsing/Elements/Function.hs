module Dreamberd.Parsing.Elements.Function (parseReturn, extractFunctionParts) where

import Dreamberd.Parsing.Utils (extractValueAndRest, getVariableName, parseScope, trimSpaces)
import Dreamberd.Parsing.Values (parseAnyValue)
import Dreamberd.Types (AstNode (Return))

extractFunctionParts :: String -> Either String (String, [String], String, String)
extractFunctionParts str =
    if not ('(' `elem` str && ')' `elem` str)
        then Left "No parenthesis found for function params"
        else
            let (nameAndParams, restAfterParams) = break (== ')') str
                (strippedName, parameters) = break (== '(') (trimSpaces nameAndParams)
                params = words $ map (\c -> if c == ',' then ' ' else c) $ tail parameters
             in getVariableName (trimSpaces strippedName) >>= \name ->
                    parseScope (drop 1 restAfterParams) >>= \(body, restOfCode) -> Right (name, params, body, restOfCode)

parseReturn :: String -> [AstNode] -> Either String (String, [AstNode])
parseReturn code ast =
    let (value, rest) = extractValueAndRest code
     in parseAnyValue value >>= \result -> Right (rest, ast ++ [Return result])
