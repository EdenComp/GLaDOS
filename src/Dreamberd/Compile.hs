module Dreamberd.Compile (
    compileAst,
) where

import Data.List (elemIndex)
import qualified Dreamberd.Types as AST
import qualified Dreamberd.Vm as VM

compileAst :: [AST.AstNode] -> Either String [VM.Insts]
compileAst [] = Left "No instructions provided"
compileAst ast = compileNodes [] ast []

compileNodes :: [String] -> [AST.AstNode] -> [VM.Insts] -> Either String [VM.Insts]
compileNodes _ [] insts = Right insts
compileNodes params (curNode : nextNodes) insts = compileNode params curNode >>= \curInsts -> compileNodes params nextNodes (insts ++ curInsts)

compileNode :: [String] -> AST.AstNode -> Either String [VM.Insts]
compileNode params (AST.Call op args) = compileCall params op args
compileNode params (AST.If test trueBody falseBody) = compileIf params test trueBody falseBody
compileNode params (AST.Function name args body) = compileFunction params name args body
compileNode params (AST.Return value) = compileReturn params value
compileNode _ _ = Left "Unknown node type"

compileReturn :: [String] -> AST.AstNode -> Either String [VM.Insts]
compileReturn params (AST.Call op args) = compileCall params op args >>= \call -> Right $ call ++ [VM.Ret]
compileReturn params value = compileValuePush params value >>= \pushInst -> Right [pushInst, VM.Ret]

compileFunction :: [String] -> String -> [String] -> [AST.AstNode] -> Either String [VM.Insts]
compileFunction params name args body =
    compileNodes (args ++ params) body []
        >>= \bodyInsts -> Right [VM.DefineEnv name $ VM.Function bodyInsts]

compileCall :: [String] -> String -> [AST.AstNode] -> Either String [VM.Insts]
compileCall params "=" [AST.Identifier iden, value] = compileAssignation params iden value
compileCall params op args = compileBuiltinCall params op args <> compileCustomCall params op args

compileIf :: [String] -> AST.AstNode -> [AST.AstNode] -> [AST.AstNode] -> Either String [VM.Insts]
compileIf params (AST.Call op args) trueBody falseBody =
    compileCall params op args
        >>= \call ->
            compileNodes params trueBody []
                >>= \trueInsts ->
                    compileNodes params falseBody []
                        >>= \falseInsts ->
                            Right $ call ++ [VM.JumpIfFalse $ length trueInsts] ++ trueInsts ++ falseInsts
compileIf params test trueBody falseBody =
    ( compileValuePush params test
        >>= \testPush ->
            compileNodes params trueBody []
                >>= \trueInsts ->
                    compileNodes params falseBody []
                        >>= \falseInsts -> Right $ [testPush, VM.JumpIfFalse $ length trueInsts] ++ trueInsts ++ falseInsts
    )
        <> Left "Unknown if type"

compileBuiltinCall :: [String] -> String -> [AST.AstNode] -> Either String [VM.Insts]
compileBuiltinCall params op [a, b] =
    getBuiltinCallForOp op
        >>= \call ->
            compileValuePush params a
                >>= \aPush ->
                    compileValuePush params b
                        >>= \bPush -> Right [bPush, aPush, VM.Push $ VM.Symbol call, VM.Call]
compileBuiltinCall _ _ _ = Left "Unknown builtin call"

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

compileCustomCall :: [String] -> String -> [AST.AstNode] -> Either String [VM.Insts]
compileCustomCall params name args = mapM (compileValuePush params) args >>= \args' -> Right $ reverse args' ++ [VM.PushEnv name, VM.Call]

compileAssignation :: [String] -> String -> AST.AstNode -> Either String [VM.Insts]
compileAssignation params iden value = compileValuePush params value >>= \pushInst -> Right [pushInst, VM.DefineEnvFromStack iden]

compileValuePush :: [String] -> AST.AstNode -> Either String VM.Insts
compileValuePush _ (AST.Boolean b) = Right $ VM.Push $ VM.Bool b
compileValuePush _ (AST.Number n) = Right $ VM.Push $ VM.Number n
compileValuePush _ (AST.String s) = Right $ VM.Push $ VM.String s
compileValuePush params (AST.Identifier i) = Right $ case elemIndex i params of
    Nothing -> VM.PushEnv i
    Just idx -> VM.PushArg idx
compileValuePush _ _ = Left "Unknown value type"
