{-# LANGUAGE RankNTypes #-}

module Ast (
  AstNode (..),
  Variable (..),
  sexprToAst,
  evalAstNode,
  evalAst,
) where

import Data.Maybe (isNothing)
import Data.Type.Coercion ()
import qualified SExpr (SymbolicExpression (List, Number, Symbol))

data AstNode
  = Number Float
  | Symbol String
  | Boolean Bool
  | Call String [AstNode]
  | Void
  deriving (Show)

data Variable = Variable
  { identifier :: String
  , value :: AstNode
  }
  deriving (Show)

sexprToAst :: SExpr.SymbolicExpression -> Maybe AstNode
sexprToAst (SExpr.Number val) = Just (Number val)
sexprToAst (SExpr.Symbol "#t") = Just (Boolean True)
sexprToAst (SExpr.Symbol "#f") = Just (Boolean False)
sexprToAst (SExpr.Symbol sym) = Just (Symbol sym)
sexprToAst (SExpr.List (SExpr.Symbol name : xs)) = Call name <$> mapM sexprToAst xs
sexprToAst (SExpr.List []) = Just (Symbol "()")
sexprToAst _ = Nothing

getNewStateOfAst :: Maybe ([AstNode], [Variable]) -> AstNode -> Maybe ([AstNode], [Variable])
getNewStateOfAst (Just (nodes, variables)) node = evalAstNode variables node >>= uncurry (filterVoidValues nodes)
getNewStateOfAst _ _ = Nothing

filterVoidValues :: [AstNode] -> AstNode -> [Variable] -> Maybe ([AstNode], [Variable])
filterVoidValues nodes Void variables = Just (nodes, variables)
filterVoidValues nodes node variables = Just (nodes ++ [node], variables)

evalAst :: [AstNode] -> Maybe [AstNode]
evalAst = fmap fst . foldl getNewStateOfAst (Just ([], []))

evalAstNode :: [Variable] -> AstNode -> Maybe (AstNode, [Variable])
evalAstNode variables (Call "+" [l, r]) = applyBinaryOpOnNodes variables l r (+) >>= \ast -> Just (ast, variables)
evalAstNode variables (Call "*" [l, r]) = applyBinaryOpOnNodes variables l r (*) >>= \ast -> Just (ast, variables)
evalAstNode variables (Call "-" [l, r]) = applyBinaryOpOnNodes variables l r (-) >>= \ast -> Just (ast, variables)
evalAstNode variables (Call "define" [Symbol iden, r]) =
  evalAstNode variables r
    >>= \(rResolved, newVariables) ->
      addVariable iden rResolved newVariables
        >>= \newNewVariables -> Just (Void, newNewVariables)
evalAstNode variables (Symbol iden) = getVariableValue iden variables >>= \ast -> Just (ast, variables)
evalAstNode variables val = Just (val, variables)

applyBinaryOpOnNodes :: [Variable] -> AstNode -> AstNode -> (forall b. (Num b) => b -> b -> b) -> Maybe AstNode
applyBinaryOpOnNodes _ (Number l) (Number r) op = Just $ Number (op l r)
applyBinaryOpOnNodes vars (Symbol l) (Symbol r) op = evalAstNode vars (Symbol l) >>= \(lResolved, newVariables) -> evalAstNode newVariables (Symbol r) >>= \(rResolved, newNewVariables) -> applyBinaryOpOnNodes newNewVariables lResolved rResolved op
applyBinaryOpOnNodes vars (Call name args) r op = evalAstNode vars (Call name args) >>= \(l, newVariables) -> applyBinaryOpOnNodes newVariables l r op
applyBinaryOpOnNodes vars (Symbol sym) r op = evalAstNode vars (Symbol sym) >>= \(l, newVariables) -> applyBinaryOpOnNodes newVariables l r op
applyBinaryOpOnNodes _ (Boolean _) _ _ = Nothing
applyBinaryOpOnNodes vars l r op = applyBinaryOpOnNodes vars r l op

getVariableValue :: [Char] -> [Variable] -> Maybe AstNode
getVariableValue searchIdentifier (x : xs)
  | identifier x == searchIdentifier = Just $ value x
  | otherwise = getVariableValue searchIdentifier xs
getVariableValue _ [] = Nothing

addVariable :: [Char] -> AstNode -> [Variable] -> Maybe [Variable]
addVariable iden varValue variables
  | isNothing $ getVariableValue iden variables = Just (variables ++ [Variable{identifier = iden, value = varValue}])
  | otherwise = Nothing
