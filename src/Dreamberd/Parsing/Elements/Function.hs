module Dreamberd.Parsing.Elements.Function (parseReturn, extractFunctionParts, parseFunctionGlobalCall) where

import Data.List (isPrefixOf)
import Dreamberd.Parsing.Utils (breakOnClosingParenthesis, extractValueAndRest, getVariableName, parseScope, trimSpaces)
import Dreamberd.Parsing.Values (parseAnyValue, parseFunctionCall)
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

parseFunctionGlobalCall :: String -> Either String (String, AstNode)
parseFunctionGlobalCall code =
    if ';' `notElem` code
        then Left "No semicolon found after function call"
        else
            breakOnClosingParenthesis code (-1) >>= \(beforeCloseParenthesis, afterCloseParenthesis) ->
                parseFunctionCall (beforeCloseParenthesis ++ ")") >>= \node ->
                    if not (";" `isPrefixOf` afterCloseParenthesis)
                        then Left "No semicolon found after function call"
                        else
                            Right
                                (drop 1 afterCloseParenthesis, node)
