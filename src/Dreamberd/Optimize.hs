module Dreamberd.Optimize (optimizeNode) where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Dreamberd.Types (AstNode (..))

optimizeNode :: AstNode -> AstNode
optimizeNode node = fromMaybe node $ optimizeCall node

optimizeCall :: AstNode -> Maybe AstNode
optimizeCall (Call op [a, b]) = optimizeAdditiveBuiltinOperation op a b <|> (optimizeComparativeBuiltinOperation op a b >>= \op' -> Just $ Boolean op')
optimizeCall _ = Nothing

optimizeAdditiveBuiltinOperation :: String -> AstNode -> AstNode -> Maybe AstNode
optimizeAdditiveBuiltinOperation op a b =
    case op of
        "+" -> Just (+)
        "-" -> Just (-)
        "*" -> Just (*)
        "/" -> Just div
        "%" -> Just mod
        _ -> Nothing
        >>= \op' -> Just $ op' a b
optimizeComparativeBuiltinOperation :: String -> AstNode -> AstNode -> Maybe Bool
optimizeComparativeBuiltinOperation op a b =
    case op of
        "==" -> Just (==)
        "!=" -> Just (/=)
        "<" -> Just (<)
        "<=" -> Just (<=)
        ">" -> Just (>)
        ">=" -> Just (>=)
        _ -> Nothing
        >>= \op' -> Just $ op' a b
