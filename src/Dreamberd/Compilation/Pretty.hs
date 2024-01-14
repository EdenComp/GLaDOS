module Dreamberd.Compilation.Pretty (
    prettyPrintAST,
) where

import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Dreamberd.Types (AstNode (..))

prettyPrintAST :: [AstNode] -> String
prettyPrintAST insts = prettyPrintNodes insts 0

prettyPrintNodes :: [AstNode] -> Int -> String
prettyPrintNodes [] _ = ""
prettyPrintNodes (Integer nbr : xs) sp = replicate sp ' ' ++ "Integer " ++ show nbr ++ "\n" ++ prettyPrintNodes xs sp
prettyPrintNodes (Float nbr : xs) sp = replicate sp ' ' ++ "Float " ++ show nbr ++ "\n" ++ prettyPrintNodes xs sp
prettyPrintNodes (Boolean b : xs) sp = replicate sp ' ' ++ "Bool " ++ show b ++ "\n" ++ prettyPrintNodes xs sp
prettyPrintNodes (String str : xs) sp = replicate sp ' ' ++ "String " ++ show str ++ "\n" ++ prettyPrintNodes xs sp
prettyPrintNodes (Identifier sym : xs) sp = replicate sp ' ' ++ "Identifier " ++ show sym ++ "\n" ++ prettyPrintNodes xs sp
prettyPrintNodes (Function name params body : xs) sp = replicate sp ' ' ++ "Function " ++ name ++ " (" ++ intercalate ", " params ++ ") {\n" ++ prettyPrintNodes body (sp + 2) ++ replicate sp ' ' ++ "}\n" ++ prettyPrintNodes xs sp
prettyPrintNodes (Call name params : xs) sp = replicate sp ' ' ++ "Call " ++ name ++ " (\n" ++ prettyPrintNodes params (sp + 2) ++ replicate sp ' ' ++ ")\n" ++ prettyPrintNodes xs sp
prettyPrintNodes (If cond trueBody falseBody : xs) sp = replicate sp ' ' ++ "If (\n" ++ prettyPrintNodes [cond] (sp + 2) ++ replicate sp ' ' ++ ") {\n" ++ prettyPrintNodes trueBody (sp + 2) ++ replicate sp ' ' ++ "} else {\n" ++ prettyPrintNodes falseBody (sp + 2) ++ replicate sp ' ' ++ "}\n" ++ prettyPrintNodes xs sp
prettyPrintNodes (Return val : xs) sp = replicate sp ' ' ++ "Return (\n" ++ prettyPrintNodes (maybeToList val) (sp + 2) ++ replicate sp ' ' ++ ")\n" ++ prettyPrintNodes xs sp
prettyPrintNodes (Loop test body initNode updateNode : xs) sp = replicate sp ' ' ++ "Loop (\n" ++ prettyPrintNodes [test] (sp + 2) ++ "init:\n" ++ prettyPrintNodes (maybeToList initNode) (sp + 4) ++ prettyPrintNodes [test] (sp + 2) ++ "condition:\n" ++ prettyPrintNodes [test] (sp + 4) ++ prettyPrintNodes [test] (sp + 2) ++ "update:\n" ++ prettyPrintNodes (maybeToList updateNode) (sp + 4) ++ prettyPrintNodes [test] (sp + 2) ++ "body:\n" ++ prettyPrintNodes body (sp + 4) ++ replicate sp ' ' ++ ")\n" ++ prettyPrintNodes xs sp
prettyPrintNodes (List list : xs) sp = replicate sp ' ' ++ "List [\n" ++ prettyPrintNodes list (sp + 2) ++ replicate sp ' ' ++ "]\n" ++ prettyPrintNodes xs sp
prettyPrintNodes (Import path : xs) sp = replicate sp ' ' ++ "Import " ++ show path ++ "\n" ++ prettyPrintNodes xs sp
