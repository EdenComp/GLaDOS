module Dreamberd.Parsing.Elements.Variable (parseVar) where

import Dreamberd.Parsing.Utils (extractValueAndRest, getVariableName)
import Dreamberd.Parsing.Values (parseAnyValue)
import Dreamberd.Types (AstNode (AssignVariable, Identifier, Operator))

parseVar :: Maybe String -> String -> [AstNode] -> Either String (String, [AstNode])
parseVar varType code ast =
    let
        wordsInCode = words code
     in
        if length wordsInCode < 2 || (wordsInCode !! 1) /= "="
            then Left "Expected '=' after variable name"
            else
                getVariableName (head wordsInCode) >>= \name ->
                    let
                        (value, restOfCode) = extractValueAndRest (unwords (drop 2 wordsInCode))
                     in
                        parseAnyValue value >>= \node -> case varType of
                            Just vt -> Right (restOfCode, ast ++ [AssignVariable vt name node])
                            Nothing -> Right (restOfCode, ast ++ [Operator "=" (Identifier name) node])
