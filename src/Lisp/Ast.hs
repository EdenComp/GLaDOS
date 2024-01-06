{-# LANGUAGE ImpredicativeTypes #-}

module Lisp.Ast (
    evalLispAst,
) where

import Lisp.Function (evalCall)
import Lisp.Types (AstNode (..), Variable (..))
import Lisp.Variable (getVariableValue)

getNewStateOfAst :: Maybe ([AstNode], [Variable]) -> AstNode -> Maybe ([AstNode], [Variable])
getNewStateOfAst (Just (nodes, variables)) node = evalAstNode variables node >>= uncurry (filterVoidValues nodes)
getNewStateOfAst _ _ = Nothing

filterVoidValues :: [AstNode] -> AstNode -> [Variable] -> Maybe ([AstNode], [Variable])
filterVoidValues nodes Void variables = Just (nodes, variables)
filterVoidValues nodes node variables = Just (nodes ++ [node], variables)

evalLispAst :: [AstNode] -> Maybe [AstNode]
evalLispAst = fmap fst . foldl getNewStateOfAst (Just ([], []))

evalAstNode :: [Variable] -> AstNode -> Maybe (AstNode, [Variable])
evalAstNode variables (Call symbol args) = evalCall variables symbol args evalAstNode
evalAstNode variables (Symbol iden) = getVariableValue iden variables >>= \ast -> Just (ast, variables)
evalAstNode variables val = Just (val, variables)
