module Dreamberd.Compile (
    compileAst,
    compileExecAst,
) where

import Control.Monad ((>=>))
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
compileNode (AST.Return value) = compileValue value >>= \r -> Right [VM.Push r, VM.Ret]
compileNode _ = Left "Unknown node type"

compileCall :: String -> [AST.AstNode] -> Either String [VM.Insts]
compileCall "=" [AST.Identifier iden, value] = compileAssignation iden value
compileCall op args = compileBuiltinCall op args <> compileCustomCall op args

compileBuiltinCall :: String -> [AST.AstNode] -> Either String [VM.Insts]
compileBuiltinCall op [a, b] =
    getBuiltinCallForOp op
        >>= \call ->
            compileValue a
                >>= \a' ->
                    compileValue b
                        >>= \b' -> Right [VM.Push b', VM.Push a', VM.Push $ VM.Symbol call, VM.Call]
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
compileCustomCall name args = mapM (compileValue >=> (Right . VM.Push)) args >>= \args' -> Right $ reverse args' ++ [VM.PushEnv name, VM.Call]

compileAssignation :: String -> AST.AstNode -> Either String [VM.Insts]
compileAssignation iden v = compileValue v >>= \r -> Right [VM.DefineEnv iden $ VM.Variable r]

compileValue :: AST.AstNode -> Either String VM.Value
compileValue (AST.Number val) = Right $ VM.Number val
compileValue (AST.Boolean val) = Right $ VM.Bool val
compileValue (AST.String val) = Right $ VM.String val
compileValue _ = Left "Unknown value type"
