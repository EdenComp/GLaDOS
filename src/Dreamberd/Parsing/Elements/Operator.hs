module Dreamberd.Parsing.Elements.Operator (parseExpression) where

import Dreamberd.Parsing.Values (parseAnyValue)
import Dreamberd.Types (AstNode (Operator))

parseExpression :: String -> Either String AstNode
parseExpression str =
    case words str of
        [lhs, op, rhs] ->
            parseAnyValue lhs >>= \lhsNode ->
                parseOperator op >>= \opNode ->
                    parseAnyValue rhs >>= \rhsNode ->
                        Right (Operator opNode lhsNode rhsNode)
        _ -> Left "Invalid expression"

parseOperator :: String -> Either String String
parseOperator str =
    if str `elem` ["=", "+=", "-=", "*=", "/=", "%="]
        then Right str
        else Left "Invalid operator"
