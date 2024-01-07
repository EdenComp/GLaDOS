module Dreamberd.Parsing.Elements.Operator (parseOperator) where


import Dreamberd.Parsing.Values (parseAnyValue)
import Dreamberd.Types (AstNode (Operator, String))

parseOperator :: String -> Either String (String, String)
parseOperator str =
    let operators = ["<=", ">=", "==", "!=", "<", ">", "="]
        findOperator [] = Nothing
        findOperator (op:ops) = if op `isPrefixOf` str then Just op else findOperator ops
    in case findOperator operators of
        Just op -> Right (op, drop (length op) str)
        Nothing -> Left "No operator found"
