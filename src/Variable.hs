module Variable (getVariableValue, addVariable) where

import Debug.Trace (trace)
import Types (AstNode, Variable (..))

getVariableValue :: [Char] -> [Variable] -> Maybe AstNode
getVariableValue searchIdentifier (x : xs)
    | identifier x == searchIdentifier = Just $ value x
    | otherwise = getVariableValue searchIdentifier xs
getVariableValue _ [] = Nothing

addVariable :: [Char] -> AstNode -> [Variable] -> Maybe [Variable]
addVariable iden varValue (x : xs)
    | identifier x == iden = trace ("Setting variable " ++ iden ++ " to " ++ show varValue) Just (Variable{identifier = iden, value = varValue} : xs)
    | otherwise = (x :) <$> addVariable iden varValue xs
addVariable iden varValue [] = trace ("Setting (add) variable " ++ iden ++ " to " ++ show varValue) Just [Variable{identifier = iden, value = varValue}]
