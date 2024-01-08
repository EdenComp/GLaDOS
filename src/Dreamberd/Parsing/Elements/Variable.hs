module Dreamberd.Parsing.Elements.Variable (parseVar) where

import Dreamberd.Parsing.Utils (extractValueAndRest, getVariableName)
import Dreamberd.Parsing.Values (parseAnyValue)
import Dreamberd.Types (AstNode (Operator, String))

parseVar :: String -> String -> [AstNode] -> Either String (String, [AstNode])
parseVar _ code ast =
    let
        wordsInCode = words code
     in
        if length wordsInCode < 2 || (wordsInCode !! 1) /= "="
            then Left "Expected '=' after variable name"
            else case getVariableName (head wordsInCode) of
                Right name ->
                    let
                        (value, restOfCode) = extractValueAndRest (unwords (drop 2 wordsInCode))
                     in
                        case parseAnyValue value of
                            Right node -> Right (restOfCode, ast ++ [Operator "=" (String name) node])
                            Left err -> Left err
                Left err -> Left err
