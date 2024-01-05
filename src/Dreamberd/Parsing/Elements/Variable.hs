module Dreamberd.Parsing.Elements.Variable (parseVar) where

import Dreamberd.Parsing.Utils (extractValueAndRest, getVariableName)
import Dreamberd.Parsing.Values (parseAnyValue)
import Dreamberd.Types (AstNode (Operator, String))

parseVar :: String -> String -> [AstNode] -> Either String (String, [AstNode])
parseVar _ code ast =
    case getVariableName (head (words code)) of
        Right name ->
            let
                (value, restOfCode) = extractValueAndRest (unwords (drop 2 (words code)))
             in
                case parseAnyValue value of
                    Right node -> Right (restOfCode, ast ++ [Operator "=" (String name) node])
                    Left err -> Left err
        Left err -> Left err
