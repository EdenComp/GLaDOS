{-# LANGUAGE RankNTypes #-}

module Ast (
  AstNode (..),
  Variable (..),
  sexprToAst,
  evalAstNode,
  evalAst,
) where

import Data.Type.Coercion ()
import qualified SExpr (SymbolicExpression (Float, Integer, List, Symbol))

data AstNode
  = Integer Int
  | Float Float
  | Symbol String
  | Boolean Bool
  | Call String [AstNode]
  deriving (Show)

data Variable = Variable
  { identifier :: String
  , value :: AstNode
  }
  deriving (Show)

sexprToAst :: SExpr.SymbolicExpression -> Maybe AstNode
sexprToAst (SExpr.Integer intValue) = Just (Integer intValue)
sexprToAst (SExpr.Float floatValue) = Just (Float floatValue)
sexprToAst (SExpr.Symbol "#t") = Just (Boolean True)
sexprToAst (SExpr.Symbol "#f") = Just (Boolean False)
sexprToAst (SExpr.Symbol sym) = Just (Symbol sym)
sexprToAst (SExpr.List (SExpr.Symbol name : xs)) = Call name <$> mapM sexprToAst xs
sexprToAst (SExpr.List []) = Just (Symbol "()")
sexprToAst _ = Nothing

getNewStateOfAst :: Maybe ([AstNode], [Variable]) -> AstNode -> Maybe ([AstNode], [Variable])
getNewStateOfAst (Just (nodes, variables)) node = evalAstNode variables node >>= \(ast, newVariables) -> Just (nodes ++ [ast], newVariables)
getNewStateOfAst _ _ = Nothing

evalAst :: [AstNode] -> Maybe [AstNode]
evalAst = fmap fst . foldl getNewStateOfAst (Just ([], []))

evalAstNode :: [Variable] -> AstNode -> Maybe (AstNode, [Variable])
evalAstNode variables (Call "+" [l, r]) = applyBinaryOpOnNodes variables l r (+) >>= \ast -> Just (ast, variables)
evalAstNode variables (Call "*" [l, r]) = applyBinaryOpOnNodes variables l r (*) >>= \ast -> Just (ast, variables)
evalAstNode variables (Call "-" [l, r]) = applyBinaryOpOnNodes variables l r (-) >>= \ast -> Just (ast, variables)
evalAstNode variables (Call "define" [Symbol iden, r]) = Just (Call "defined" [Symbol iden, r], addVariable iden r variables)
evalAstNode variables (Symbol iden) = getVariableValue iden variables >>= \ast -> Just (ast, variables)
evalAstNode _ _ = Nothing

applyBinaryOpOnNodes :: [Variable] -> AstNode -> AstNode -> (forall b. (Num b) => b -> b -> b) -> Maybe AstNode
applyBinaryOpOnNodes _ (Integer l) (Integer r) op = Just $ Integer (op l r)
applyBinaryOpOnNodes _ (Integer l) (Float r) op = Just $ Float (op (fromIntegral l) r)
applyBinaryOpOnNodes _ (Float l) (Float r) op = Just $ Float (op l r)
applyBinaryOpOnNodes _ (Float l) (Integer r) op = Just $ Float (op l (fromIntegral r))
applyBinaryOpOnNodes variables (Call name args) (Integer r) op = evalAstNode variables (Call name args) >>= \(l, newVariables) -> applyBinaryOpOnNodes newVariables l (Integer r) op
applyBinaryOpOnNodes variables (Integer l) (Call name args) op = evalAstNode variables (Call name args) >>= \(r, newVariables) -> applyBinaryOpOnNodes newVariables (Integer l) r op
applyBinaryOpOnNodes variables (Call name args) (Float r) op = evalAstNode variables (Call name args) >>= \(l, newVariables) -> applyBinaryOpOnNodes newVariables l (Float r) op
applyBinaryOpOnNodes variables (Float l) (Call name args) op = evalAstNode variables (Call name args) >>= \(r, newVariables) -> applyBinaryOpOnNodes newVariables (Float l) r op
applyBinaryOpOnNodes variables (Symbol sym) (Integer r) op = evalAstNode variables (Symbol sym) >>= \(l, newVariables) -> applyBinaryOpOnNodes newVariables l (Integer r) op
applyBinaryOpOnNodes variables (Integer l) (Symbol sym) op = evalAstNode variables (Symbol sym) >>= \(r, newVariables) -> applyBinaryOpOnNodes newVariables (Integer l) r op
applyBinaryOpOnNodes variables (Symbol sym) (Float r) op = evalAstNode variables (Symbol sym) >>= \(l, newVariables) -> applyBinaryOpOnNodes newVariables l (Float r) op
applyBinaryOpOnNodes variables (Float l) (Symbol sym) op = evalAstNode variables (Symbol sym) >>= \(r, newVariables) -> applyBinaryOpOnNodes newVariables (Float l) r op
applyBinaryOpOnNodes _ _ _ _ = Nothing

getVariableValue :: [Char] -> [Variable] -> Maybe AstNode
getVariableValue searchIdentifier (x : xs)
  | identifier x == searchIdentifier = Just $ value x
  | otherwise = getVariableValue searchIdentifier xs
getVariableValue _ [] = Nothing

addVariable :: [Char] -> AstNode -> [Variable] -> [Variable]
addVariable iden varValue variables = variables ++ [Variable{identifier = iden, value = varValue}]