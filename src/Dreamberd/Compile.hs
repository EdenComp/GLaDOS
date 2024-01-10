module Dreamberd.Compile (
    compileAst,
    compileExecAst,
) where

import qualified Dreamberd.Types as AST
import qualified Dreamberd.Vm as VM

compileExecAst :: [AST.AstNode] -> IO (Either String VM.Value)
compileExecAst ast = either (pure . Left) (VM.exec [] [] []) (compileAst ast)

compileAst :: [AST.AstNode] -> Either String [VM.Insts]
compileAst [] = Left "No instructions provided"
compileAst ast = compileNodes ast []

compileNodes :: [AST.AstNode] -> [VM.Insts] -> Either String [VM.Insts]
compileNodes [] insts = Right insts
compileNodes (curNode : nextNodes) insts = compileNode curNode >>= \curInsts -> compileNodes nextNodes (insts ++ curInsts)

compileNode :: AST.AstNode -> Either String [VM.Insts]
compileNode (AST.Call op args) = compileCall op args
compileNode (AST.Operator op l r) = compileNode (AST.Call op [l, r]) -- TODO: Remove this when Operators are removed
compileNode (AST.AssignVariable _ name value) = compileNode (AST.Call "=" [AST.Identifier name, value]) -- TODO: Remove this when AssginVariable
compileNode (AST.If (AST.Operator op l r) t f) = compileNode (AST.If (AST.Call op [l, r]) t f) -- TODO: Remove this when Operators are removed
compileNode (AST.If test trueBody falseBody) = compileIf test trueBody falseBody
compileNode _ = Left "Unknown node type"

compileCall :: String -> [AST.AstNode] -> Either String [VM.Insts]
compileCall "=" [AST.Identifier iden, value] = compileAssignation iden value
compileCall op args = compileBuiltinCall op args <> compileCustomCall op args

compileIf :: AST.AstNode -> [AST.AstNode] -> [AST.AstNode] -> Either String [VM.Insts]
compileIf (AST.Call op args) trueBody falseBody =
    compileCall op args
        >>= \call ->
            compileNodes trueBody []
                >>= \trueInsts ->
                    compileNodes falseBody []
                        >>= \falseInsts ->
                            Right $ call ++ [VM.JumpIfFalse $ length trueInsts] ++ trueInsts ++ falseInsts
compileIf test trueBody falseBody =
    ( compileValuePush test
        >>= \testPush ->
            compileNodes trueBody []
                >>= \trueInsts ->
                    compileNodes falseBody []
                        >>= \falseInsts -> Right $ [testPush, VM.JumpIfFalse $ length trueInsts] ++ trueInsts ++ falseInsts
    )
        <> Left "Unknown if type"

compileBuiltinCall :: String -> [AST.AstNode] -> Either String [VM.Insts]
compileBuiltinCall op [a, b] =
    getBuiltinCallForOp op
        >>= \call ->
            compileValuePush a
                >>= \aPush ->
                    compileValuePush b
                        >>= \bPush -> Right [bPush, aPush, VM.Push $ VM.Symbol call, VM.Call]
compileBuiltinCall _ _ = Left "Unknown builtin call"

getBuiltinCallForOp :: String -> Either String VM.Call
getBuiltinCallForOp "+" = Right VM.Add
getBuiltinCallForOp "-" = Right VM.Sub
getBuiltinCallForOp "*" = Right VM.Mul
getBuiltinCallForOp "/" = Right VM.Div
getBuiltinCallForOp "==" = Right VM.Eq
getBuiltinCallForOp "!=" = Right VM.Neq
getBuiltinCallForOp "<" = Right VM.Less
getBuiltinCallForOp "<=" = Right VM.LessOrEqual
getBuiltinCallForOp ">" = Right VM.Greater
getBuiltinCallForOp ">=" = Right VM.GreaterOrEqual
getBuiltinCallForOp _ = Left "Unknown builtin call"

compileCustomCall :: String -> [AST.AstNode] -> Either String [VM.Insts]
compileCustomCall name args = mapM compileValuePush args >>= \args' -> Right $ reverse args' ++ [VM.PushEnv name, VM.Call]

compileAssignation :: String -> AST.AstNode -> Either String [VM.Insts]
compileAssignation iden value = compileValuePush value >>= \pushInst -> Right [pushInst, VM.DefineEnvFromStack iden]

compileValuePush :: AST.AstNode -> Either String VM.Insts
compileValuePush (AST.Boolean b) = Right $ VM.Push $ VM.Bool b
compileValuePush (AST.Number n) = Right $ VM.Push $ VM.Number n
compileValuePush (AST.String s) = Right $ VM.Push $ VM.String s
compileValuePush (AST.Identifier i) = Right $ VM.PushEnv i
compileValuePush _ = Left "Unknown value type"
