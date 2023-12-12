{-# LANGUAGE ImpredicativeTypes #-}

module Function (appendParametersToVariables, evalCall) where

import Types (AstNode (..), BuiltinOperator, NodeEvaluator, Variable (..))
import Variable (addVariable)

appendParametersToVariables :: [Variable] -> [String] -> [AstNode] -> Maybe [Variable]
appendParametersToVariables vars parameters args
    | length args /= length parameters = Nothing
    | otherwise = Just $ vars ++ zipWith (\param arg -> Variable{identifier = param, value = arg}) parameters args

evalCall :: [Variable] -> AstNode -> [AstNode] -> NodeEvaluator -> Maybe (AstNode, [Variable])
evalCall vars (Symbol "lambda") [Call arg rest, body] _ = getLambdaParameters (arg : rest) >>= \parameters -> Just (Lambda parameters body, vars)
evalCall vars (Lambda parameters body) args nodeEvaluator = appendParametersToVariables vars parameters args >>= \newVariables -> nodeEvaluator newVariables body >>= \(ast, newNewVariables) -> Just (ast, newNewVariables)
evalCall vars (Symbol "define") [Symbol iden, val] nodeEvaluator =
    nodeEvaluator vars val
        >>= \(rResolved, newVariables) ->
            addVariable iden rResolved newVariables
                >>= \newNewVariables -> Just (Void, newNewVariables)
evalCall vars (Symbol op) [l, r] nodeEvaluator = getOperatorForBinaryOpSymbol op >>= \resolvedOp -> applyBinaryOpOnNodes vars l r resolvedOp nodeEvaluator >>= \ast -> Just (ast, vars)
evalCall vars func args nodeEvaluator = nodeEvaluator vars func >>= \(resolvedFunc, newVariables) -> evalCall newVariables resolvedFunc args nodeEvaluator

getLambdaParameters :: [AstNode] -> Maybe [String]
getLambdaParameters [] = Just []
getLambdaParameters ((Symbol iden) : rest) = (iden :) <$> getLambdaParameters rest
getLambdaParameters _ = Nothing

getOperatorForBinaryOpSymbol :: [Char] -> Maybe BuiltinOperator
getOperatorForBinaryOpSymbol "+" = Just (+)
getOperatorForBinaryOpSymbol "*" = Just (*)
getOperatorForBinaryOpSymbol "-" = Just (-)
getOperatorForBinaryOpSymbol "/" = Just div
getOperatorForBinaryOpSymbol "%" = Just mod
getOperatorForBinaryOpSymbol _ = Nothing

applyBinaryOpOnNodes :: [Variable] -> AstNode -> AstNode -> BuiltinOperator -> NodeEvaluator -> Maybe AstNode
applyBinaryOpOnNodes _ (Number l) (Number r) op _ = Just $ Number (op l r)
applyBinaryOpOnNodes vars (Symbol l) (Symbol r) op nodeEvaluator = nodeEvaluator vars (Symbol l) >>= \(lResolved, newVariables) -> nodeEvaluator newVariables (Symbol r) >>= \(rResolved, newNewVariables) -> applyBinaryOpOnNodes newNewVariables lResolved rResolved op nodeEvaluator
applyBinaryOpOnNodes vars (Call name args) r op nodeEvaluator = nodeEvaluator vars (Call name args) >>= \(l, newVariables) -> applyBinaryOpOnNodes newVariables l r op nodeEvaluator
applyBinaryOpOnNodes vars (Symbol sym) r op nodeEvaluator = nodeEvaluator vars (Symbol sym) >>= \(l, newVariables) -> applyBinaryOpOnNodes newVariables l r op nodeEvaluator
applyBinaryOpOnNodes _ (Boolean _) _ _ _ = Nothing
applyBinaryOpOnNodes vars l r op nodeEvaluator = applyBinaryOpOnNodes vars r l op nodeEvaluator
