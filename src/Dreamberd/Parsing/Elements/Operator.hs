module Dreamberd.Parsing.Elements.Operator (parseOperator) where

import Data.Char (isSpace)
import Data.List (isPrefixOf)

parseOperator :: String -> Either String (String, String)
parseOperator str =
    let operators = ["<=", ">=", "==", "!=", "<", ">", "="]
        findOperator [] = Nothing
        findOperator (op : ops) = if op `isPrefixOf` noSpaceStr then Just op else findOperator ops
        noSpaceStr = filter (not . isSpace) str
     in case findOperator operators of
            Just op -> Right (op, dropWhile isSpace . drop (length op) $ str)
            Nothing -> Left "No operator found"
