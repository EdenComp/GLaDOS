{-# LANGUAGE ImpredicativeTypes #-}

module Function (appendParametersToVariables, evalCall, applyBinaryOpOnNodes, getOperatorForBinaryOpSymbol, evalBinaryOperation) where

import Control.Applicative (Alternative ((<|>)))
import Types (AstNode (..), BuiltinOperator, BuiltinComparisonOperator, NodeEvaluator, Variable (..))
import Variable (addVariable)

appendParametersToVariables :: [Variable] -> [String] -> [AstNode] -> Maybe [Variable]
appendParametersToVariables vars parameters args
    | length args /= length parameters = Nothing
    | otherwise = Just $ vars ++ zipWith (\param arg -> Variable{identifier = param, value = arg}) parameters args

evalCall :: [Variable] -> AstNode -> [AstNode] -> NodeEvaluator -> Maybe (AstNode, [Variable])
evalCall vars (Symbol "lambda") [Call arg rest, body] _ = getLambdaParameters (arg : rest) >>= \parameters -> Just (Lambda parameters body, vars)
evalCall vars (Lambda parameters body) args nodeEvaluator = mapM (nodeEvaluator vars) args >>= \resolvedArgs -> appendParametersToVariables vars parameters (map fst resolvedArgs) >>= \newVariables -> nodeEvaluator newVariables body
evalCall vars (Symbol "define") args nodeEvaluator = evalDefine vars args nodeEvaluator >>= \newVariables -> Just (Void, newVariables)
evalCall vars (Call (Symbol "lambda") [Call arg rest, body]) args nodeEvaluator = evalCall vars (Symbol "lambda") [Call arg rest, body] nodeEvaluator >>= \(ast, newVariables) -> evalCall newVariables ast args nodeEvaluator
evalCall vars (Symbol "if") args nodeEvaluator = evalIf vars args nodeEvaluator
evalCall vars func args nodeEvaluator = evalBinaryOperation vars func args nodeEvaluator <|> evalComparisonOperation vars func args nodeEvaluator <|> evalDefinedFunction vars func args nodeEvaluator

evalBinaryOperation :: [Variable] -> AstNode -> [AstNode] -> NodeEvaluator -> Maybe (AstNode, [Variable])
evalBinaryOperation vars (Symbol op) [l, r] nodeEvaluator = getOperatorForBinaryOpSymbol op >>= \resolvedOp -> applyBinaryOpOnNodes vars l r resolvedOp nodeEvaluator >>= \ast -> Just (ast, vars)
evalBinaryOperation _ _ _ _ = Nothing

evalComparisonOperation :: [Variable] -> AstNode -> [AstNode] -> NodeEvaluator -> Maybe (AstNode, [Variable])
evalComparisonOperation vars (Symbol op) [l, r] nodeEvaluator = getOperatorForBinaryCmpSymbol op >>= \resolvedOp -> applyBinaryCmpOnNodes vars l r resolvedOp nodeEvaluator >>= \ast -> Just (ast, vars)
evalComparisonOperation _ _ _ _ = Nothing

evalDefinedFunction :: [Variable] -> AstNode -> [AstNode] -> NodeEvaluator -> Maybe (AstNode, [Variable])
evalDefinedFunction vars (Symbol iden) args nodeEvaluator = nodeEvaluator vars (Symbol iden) >>= \(ast, newVariables) -> evalCall newVariables ast args nodeEvaluator
evalDefinedFunction _ _ _ _ = Nothing

evalDefine :: [Variable] -> [AstNode] -> NodeEvaluator -> Maybe [Variable]
evalDefine vars [Symbol iden, val] nodeEvaluator =
    nodeEvaluator vars val
        >>= \(rResolved, newVariables) ->
            addVariable iden rResolved newVariables
                >>= \newNewVariables -> Just newNewVariables
evalDefine vars [Call (Symbol iden) (x : xs), body] nodeEvaluator =
    nodeEvaluator vars (Call (Symbol "lambda") [Call x xs, body])
        >>= \(rResolved, newVariables) ->
            addVariable iden rResolved newVariables
                >>= \newNewVariables -> Just newNewVariables
evalDefine _ _ _ = Nothing

evalIf :: [Variable] -> [AstNode] -> NodeEvaluator -> Maybe (AstNode, [Variable])
evalIf vars [condition, thenBranch, elseBranch] nodeEvaluator =
    nodeEvaluator vars condition
        >>= \(rResolved, newVariables) ->
            case rResolved of
                Boolean False -> nodeEvaluator newVariables elseBranch >>= \(ast, newNewVariables) -> Just (ast, newNewVariables)
                _ -> nodeEvaluator newVariables thenBranch >>= \(ast, newNewVariables) -> Just (ast, newNewVariables)
evalIf _ _ _ = Nothing

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
getOperatorForBinaryOpSymbol "div" = Just div
getOperatorForBinaryOpSymbol "mod" = Just mod
getOperatorForBinaryOpSymbol _ = Nothing

getOperatorForBinaryCmpSymbol :: [Char] -> Maybe BuiltinComparisonOperator
getOperatorForBinaryCmpSymbol "<" = Just (<)
getOperatorForBinaryCmpSymbol ">" = Just (>)
getOperatorForBinaryCmpSymbol "<=" = Just (<=)
getOperatorForBinaryCmpSymbol ">=" = Just (>=)
getOperatorForBinaryCmpSymbol "=" = Just (==)
getOperatorForBinaryCmpSymbol "eq?" = Just (==)
getOperatorForBinaryCmpSymbol _ = Nothing

applyBinaryOpOnNodes :: [Variable] -> AstNode -> AstNode -> BuiltinOperator -> NodeEvaluator -> Maybe AstNode
applyBinaryOpOnNodes _ (Number l) (Number r) op _ = Just $ Number (op l r)
applyBinaryOpOnNodes vars (Symbol l) (Symbol r) op nodeEvaluator = nodeEvaluator vars (Symbol l) >>= \(lResolved, newVariables) -> nodeEvaluator newVariables (Symbol r) >>= \(rResolved, newNewVariables) -> applyBinaryOpOnNodes newNewVariables lResolved rResolved op nodeEvaluator
applyBinaryOpOnNodes vars (Call name args) r op nodeEvaluator = nodeEvaluator vars (Call name args) >>= \(l, newVariables) -> applyBinaryOpOnNodes newVariables l r op nodeEvaluator
applyBinaryOpOnNodes vars (Symbol sym) r op nodeEvaluator = nodeEvaluator vars (Symbol sym) >>= \(l, newVariables) -> applyBinaryOpOnNodes newVariables l r op nodeEvaluator
applyBinaryOpOnNodes _ (Boolean _) _ _ _ = Nothing
applyBinaryOpOnNodes vars l r op nodeEvaluator = applyBinaryOpOnNodes vars r l op nodeEvaluator

applyBinaryCmpOnNodes :: [Variable] -> AstNode -> AstNode -> BuiltinComparisonOperator -> NodeEvaluator -> Maybe AstNode
applyBinaryCmpOnNodes _ (Number l) (Number r) op _ = Just $ Boolean (op l r)
applyBinaryCmpOnNodes vars (Symbol l) (Symbol r) op nodeEvaluator = nodeEvaluator vars (Symbol l) >>= \(lResolved, newVariables) -> nodeEvaluator newVariables (Symbol r) >>= \(rResolved, newNewVariables) -> applyBinaryCmpOnNodes newNewVariables lResolved rResolved op nodeEvaluator
applyBinaryCmpOnNodes vars (Call name args) r op nodeEvaluator = nodeEvaluator vars (Call name args) >>= \(l, newVariables) -> applyBinaryCmpOnNodes newVariables l r op nodeEvaluator
applyBinaryCmpOnNodes vars (Symbol sym) r op nodeEvaluator = nodeEvaluator vars (Symbol sym) >>= \(l, newVariables) -> applyBinaryCmpOnNodes newVariables l r op nodeEvaluator
applyBinaryCmpOnNodes _ (Boolean _) _ _ _ = Nothing
applyBinaryCmpOnNodes vars l r op nodeEvaluator = applyBinaryCmpOnNodes vars r l op nodeEvaluator
