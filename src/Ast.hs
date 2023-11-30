{-# LANGUAGE RankNTypes #-}

module Ast (
  AstNode (..),
  Variable (..),
  sexprToAst,
  evalAstNode,
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

sexprToAst :: SExpr.SymbolicExpression -> Maybe AstNode
sexprToAst (SExpr.Integer intValue) = Just (Integer intValue)
sexprToAst (SExpr.Float floatValue) = Just (Float floatValue)
sexprToAst (SExpr.Symbol "#t") = Just (Boolean True)
sexprToAst (SExpr.Symbol "#f") = Just (Boolean False)
sexprToAst (SExpr.Symbol sym) = Just (Symbol sym)
sexprToAst (SExpr.List (SExpr.Symbol name : xs)) = Call name <$> mapM sexprToAst xs
sexprToAst (SExpr.List []) = Just (Symbol "()")
sexprToAst _ = Nothing

evalAstNode :: [Variable] -> AstNode -> Maybe AstNode
evalAstNode variables (Call "+" [l, r]) = applyBinaryOpOnNodes variables l r (+)
evalAstNode variables (Call "*" [l, r]) = applyBinaryOpOnNodes variables l r (*)
evalAstNode variables (Call "-" [l, r]) = applyBinaryOpOnNodes variables l r (-)
evalAstNode variables (Symbol iden) = getVariableValue iden variables
evalAstNode _ _ = Nothing

applyBinaryOpOnNodes :: [Variable] -> AstNode -> AstNode -> (forall b. (Num b) => b -> b -> b) -> Maybe AstNode
applyBinaryOpOnNodes _ (Integer l) (Integer r) op = Just $ Integer (op l r)
applyBinaryOpOnNodes _ (Integer l) (Float r) op = Just $ Float (op (fromIntegral l) r)
applyBinaryOpOnNodes _ (Float l) (Float r) op = Just $ Float (op l r)
applyBinaryOpOnNodes _ (Float l) (Integer r) op = Just $ Float (op l (fromIntegral r))
applyBinaryOpOnNodes variables (Call name args) (Integer r) op = evalAstNode variables (Call name args) >>= \l -> applyBinaryOpOnNodes variables l (Integer r) op
applyBinaryOpOnNodes variables (Integer l) (Call name args) op = evalAstNode variables (Call name args) >>= \r -> applyBinaryOpOnNodes variables (Integer l) r op
applyBinaryOpOnNodes variables (Call name args) (Float r) op = evalAstNode variables (Call name args) >>= \l -> applyBinaryOpOnNodes variables l (Float r) op
applyBinaryOpOnNodes variables (Float l) (Call name args) op = evalAstNode variables (Call name args) >>= \r -> applyBinaryOpOnNodes variables (Float l) r op
applyBinaryOpOnNodes variables (Symbol sym) (Integer r) op = evalAstNode variables (Symbol sym) >>= \l -> applyBinaryOpOnNodes variables l (Integer r) op
applyBinaryOpOnNodes variables (Integer l) (Symbol sym) op = evalAstNode variables (Symbol sym) >>= \r -> applyBinaryOpOnNodes variables (Integer l) r op
applyBinaryOpOnNodes variables (Symbol sym) (Float r) op = evalAstNode variables (Symbol sym) >>= \l -> applyBinaryOpOnNodes variables l (Float r) op
applyBinaryOpOnNodes variables (Float l) (Symbol sym) op = evalAstNode variables (Symbol sym) >>= \r -> applyBinaryOpOnNodes variables (Float l) r op
applyBinaryOpOnNodes _ _ _ _ = Nothing

getVariableValue :: [Char] -> [Variable] -> Maybe AstNode
getVariableValue searchIdentifier (x : xs)
  | identifier x == searchIdentifier = Just $ value x
  | otherwise = getVariableValue searchIdentifier xs
getVariableValue _ [] = Nothing