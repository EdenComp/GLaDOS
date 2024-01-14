module Dreamberd.Compilation.Compile (
    compileAst,
    compileValuePush,
) where

import Data.Maybe (mapMaybe)
import qualified Dreamberd.Types as AST
import qualified Dreamberd.Vm as VM

compileAst :: [AST.AstNode] -> Either String [VM.Insts]
compileAst [] = Left "No instructions provided"
compileAst ast = compileNodes [] ast []

compileNodes :: [String] -> [AST.AstNode] -> [VM.Insts] -> Either String [VM.Insts]
compileNodes _ [] insts = Right insts
compileNodes params (curNode : nextNodes) insts = compileNode params curNode >>= \curInsts -> compileNodes params nextNodes (insts ++ curInsts)

compileScopeNodes :: [String] -> [AST.AstNode] -> [VM.Insts] -> Either String [VM.Insts]
compileScopeNodes _ [] insts = Right insts
compileScopeNodes params (curNode : nextNodes) insts = compileNode params curNode >>= (\curInsts -> compileScopeNodes params nextNodes (insts ++ curInsts)) >>= \finalInsts -> Right $ finalInsts ++ getScopedInstructions finalInsts

compileNode :: [String] -> AST.AstNode -> Either String [VM.Insts]
compileNode params (AST.Call op args) = compileCall params op args
compileNode params (AST.If test trueBody falseBody) = compileIf params test trueBody falseBody
compileNode params (AST.Function name args body) = compileFunction params name args body
compileNode params (AST.Return value) = compileReturn params value
compileNode params (AST.Loop test body initNode updateNode) = compileLoop params test body initNode updateNode
compileNode params (AST.Scope body) = compileScopeNodes params body [] >>= \insts -> Right $ insts ++ getScopedInstructions insts
compileNode _ node = (compileValuePush node >>= \inst -> Right [inst]) <> Left "Unknown node type"

compileLoop :: [String] -> AST.AstNode -> [AST.AstNode] -> Maybe AST.AstNode -> Maybe AST.AstNode -> Either String [VM.Insts]
compileLoop params test body initNode updateNode =
    compileNode params test
        >>= \testInsts ->
            compileNodes params body []
                >>= \bodyInsts ->
                    compileMaybeNode initNode
                        >>= \initInsts ->
                            compileMaybeNode updateNode
                                >>= \updateInsts ->
                                    Right $
                                        initInsts
                                            ++ testInsts
                                            ++ [VM.Jump (length bodyInsts + length updateInsts + 1) $ Just False]
                                            ++ bodyInsts
                                            ++ updateInsts
                                            ++ [VM.Jump (-(length bodyInsts + length updateInsts + length testInsts + 2)) Nothing]
                                            ++ getScopedInstructions initInsts
  where
    compileMaybeNode = maybe (Right []) (compileNode params)

compileReturn :: [String] -> Maybe AST.AstNode -> Either String [VM.Insts]
compileReturn params (Just (AST.Call op args)) = compileCall params op args >>= \call -> Right $ call ++ [VM.Ret]
compileReturn params (Just value) = compileNode params value >>= \pushInst -> Right $ pushInst ++ [VM.Ret]
compileReturn _ Nothing = Right [VM.Push VM.Void, VM.Ret]

compileFunction :: [String] -> String -> [String] -> [AST.AstNode] -> Either String [VM.Insts]
compileFunction params name args body =
    compileNodes (args ++ params) body []
        >>= \bodyInsts -> Right [VM.DefineEnv name VM.Define (Just $ VM.Function (length args) (concatMap (\nb -> [VM.PushArg nb, VM.DefineEnv (args !! nb) VM.Override Nothing]) [0 .. (length args - 1)] ++ bodyInsts ++ map VM.EraseEnv args))]

compileCall :: [String] -> String -> [AST.AstNode] -> Either String [VM.Insts]
compileCall params "=" [_, AST.Identifier iden, value] = compileAssignation params iden value False
compileCall params "=" [AST.Identifier iden, value] = compileAssignation params iden value True
compileCall params "-" [node] = compileBuiltinCall params "*" [node, AST.Integer (-1)]
compileCall params "+" [node] = compileNode params node
compileCall params [op, '='] [AST.Identifier iden, value]
    | op `elem` "+-*/%" =
        compileBuiltinCall params [op] [AST.Identifier iden, value]
            >>= \opInsts -> Right $ opInsts ++ [VM.DefineEnv iden VM.Redefine Nothing]
compileCall params [op, op2, '='] [AST.Identifier iden, value]
    | op `elem` "&|" =
        compileBuiltinCall params [op, op2] [AST.Identifier iden, value]
            >>= \opInsts -> Right $ opInsts ++ [VM.DefineEnv iden VM.Redefine Nothing]
compileCall params [opA, opB] [AST.Identifier iden]
    | opA `elem` "+-" && opA == opB =
        (++) <$> compileBuiltinCall params [opA] [AST.Identifier iden, AST.Integer 1] <*> Right [VM.DefineEnv iden VM.Redefine Nothing, VM.PushEnv iden]
compileCall params op args = compileBuiltinCall params op args <> compileCustomCall params op args

getScopedInstructions :: [VM.Insts] -> [VM.Insts]
getScopedInstructions insts = map VM.EraseEnv (mapMaybe getIdentifierFromInst insts)
  where
    getIdentifierFromInst (VM.DefineEnv iden VM.Define _) = Just iden
    getIdentifierFromInst _ = Nothing

compileIf :: [String] -> AST.AstNode -> [AST.AstNode] -> [AST.AstNode] -> Either String [VM.Insts]
compileIf params test trueBody falseBody =
    ( conditionsInsts
        >>= \condition ->
            compileScopeNodes params trueBody []
                >>= \trueInsts ->
                    compileScopeNodes params falseBody []
                        >>= \falseInsts ->
                            Right $
                                condition ++ case length falseInsts of
                                    0 -> VM.Jump (length trueInsts) (Just False) : trueInsts
                                    n ->
                                        [VM.Jump (length trueInsts + 1) $ Just False]
                                            ++ trueInsts
                                            ++ [VM.Jump n Nothing]
                                            ++ falseInsts
    )
        <> Left "Unknown if type"
  where
    conditionsInsts = case test of
        (AST.Call op args) -> compileCall params op args
        _ -> compileNode params test

compileBuiltinCall :: [String] -> String -> [AST.AstNode] -> Either String [VM.Insts]
compileBuiltinCall params op [a, b] =
    getBuiltinCallForOp op
        >>= \call ->
            compileNode params a
                >>= \aPush ->
                    compileNode params b
                        >>= \bPush -> Right $ bPush ++ aPush ++ [VM.Push $ VM.Symbol $ VM.Builtin call, VM.Call]
compileBuiltinCall _ _ _ = Left "Unknown builtin call"

getBuiltinCallForOp :: String -> Either String VM.Operator
getBuiltinCallForOp "+" = Right VM.Add
getBuiltinCallForOp "-" = Right VM.Sub
getBuiltinCallForOp "*" = Right VM.Mul
getBuiltinCallForOp "/" = Right VM.Div
getBuiltinCallForOp "%" = Right VM.Mod
getBuiltinCallForOp "**" = Right VM.Pow
getBuiltinCallForOp "==" = Right VM.Eq
getBuiltinCallForOp "!=" = Right VM.Neq
getBuiltinCallForOp "<" = Right VM.Less
getBuiltinCallForOp "<=" = Right VM.LessOrEqual
getBuiltinCallForOp ">" = Right VM.Greater
getBuiltinCallForOp ">=" = Right VM.GreaterOrEqual
getBuiltinCallForOp "&&" = Right VM.And
getBuiltinCallForOp "||" = Right VM.Or
getBuiltinCallForOp "^" = Right VM.Xor
getBuiltinCallForOp _ = Left "Unknown builtin call"

compileCustomCall :: [String] -> String -> [AST.AstNode] -> Either String [VM.Insts]
compileCustomCall params name args = mapM (compileNode params) args >>= \args' -> Right $ concat (reverse args') ++ [VM.PushEnv name, VM.Call]

compileAssignation :: [String] -> String -> AST.AstNode -> Bool -> Either String [VM.Insts]
compileAssignation params iden value False = compileNode params value >>= \pushInsts -> Right $ pushInsts ++ [VM.DefineEnv iden VM.Define Nothing]
compileAssignation params iden value True = compileNode params value >>= \pushInsts -> Right $ pushInsts ++ [VM.DefineEnv iden VM.Redefine Nothing]

compileValuePush :: AST.AstNode -> Either String VM.Insts
compileValuePush (AST.Boolean b) = Right $ VM.Push $ VM.Bool b
compileValuePush (AST.Integer n) = Right $ VM.Push $ VM.Integer n
compileValuePush (AST.String s) = Right $ VM.Push $ VM.String s
compileValuePush (AST.Float f) = Right $ VM.Push $ VM.Float f
compileValuePush (AST.Identifier i) = Right $ VM.PushEnv i
compileValuePush _ = Left "Unknown value type"
