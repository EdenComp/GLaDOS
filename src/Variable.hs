module Variable (getVariableValue, addVariable) where

import Data.Maybe (isNothing)
import Types (AstNode, Variable (..))

getVariableValue :: [Char] -> [Variable] -> Maybe AstNode
getVariableValue searchIdentifier (x : xs)
    | identifier x == searchIdentifier = Just $ value x
    | otherwise = getVariableValue searchIdentifier xs
getVariableValue _ [] = Nothing

addVariable :: [Char] -> AstNode -> [Variable] -> Maybe [Variable]
addVariable iden varValue variables
    | isNothing $ getVariableValue iden variables = Just (variables ++ [Variable{identifier = iden, value = varValue}])
    | otherwise = Nothing
