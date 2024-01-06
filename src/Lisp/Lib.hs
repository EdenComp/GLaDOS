module Lisp.Lib (
    evaluateAndPrintLispResult,
) where

import Lisp.Ast (evalLispAst)
import Lisp.Parsing (parseLisp)
import Lisp.SExpr (lispSexprsToAsts)
import Lisp.Types (AstNode)
import System.Exit (ExitCode (ExitFailure), exitWith)

getEvaluatedResult :: String -> Maybe [AstNode]
getEvaluatedResult sourceCode = parseLisp sourceCode >>= lispSexprsToAsts >>= evalLispAst

printEvaluatedResult :: [AstNode] -> IO ()
printEvaluatedResult = mapM_ print

evaluateAndPrintLispResult :: String -> IO ()
evaluateAndPrintLispResult sourceCode =
    case getEvaluatedResult sourceCode of
        Just result -> printEvaluatedResult result
        Nothing -> exitWith (ExitFailure 84)
