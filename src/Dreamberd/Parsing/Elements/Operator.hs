module Dreamberd.Parsing.Elements.Operator (parseExpression) where

import Dreamberd.Parsing.Values (parseAnyValue)
import Dreamberd.Types (AstNode (Operator))

parseExpression :: String -> Either String AstNode
parseExpression str =
    case words str of
        [lhs, op, rhs] ->
            case parseAnyValue lhs of
                Left err -> Left err
                Right lhsNode ->
                    case parseOperator op of
                        Left err -> Left err
                        Right opNode ->
                            case parseAnyValue rhs of
                                Left err -> Left err
                                Right rhsNode -> Right (Operator opNode lhsNode rhsNode)
        _ -> Left "Invalid expression"

parseOperator :: String -> Either String String
parseOperator str =
    let operators = ["=", "+", "-", "*", "/", "%", "+=", "-=", "*=", "/=", "%="]
     in if str `elem` operators
            then Right str
            else Left "Invalid operator"
