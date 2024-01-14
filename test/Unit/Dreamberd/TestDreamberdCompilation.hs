module Unit.Dreamberd.TestDreamberdCompilation (testDreamberdCompilation) where

import Test.HUnit (Test (..), assertEqual)

import qualified Dreamberd.Types as AST
import Dreamberd.Vm as VM

import Dreamberd.Compilation.Compile (compileBuiltinCall, compileCall, compileFunction, compileIf, compileLoop, compileNode, compileReturn, compileValuePush, getBuiltinCallForOp)

testDreamberdCompilation :: Test
testDreamberdCompilation =
    TestList
        [ testCompileValuePush
        , testGetBuiltinCallForOp
        , testCompileNode
        , testCompileReturn
        , testCompileFunction
        , testCompileLoop
        , testCompileCall
        , testCompileIf
        , testCompileBuiltinCall
        ]

testCompileValuePush :: Test
testCompileValuePush =
    TestList
        [ TestCase
            ( assertEqual
                "compile Boolean True"
                (Right (VM.Push (VM.Bool True)))
                (compileValuePush [] (AST.Boolean True))
            )
        , TestCase
            ( assertEqual
                "compile Boolean False"
                (Right (VM.Push (VM.Bool False)))
                (compileValuePush [] (AST.Boolean False))
            )
        , TestCase
            ( assertEqual
                "compile Integer"
                (Right (VM.Push (VM.Integer 42)))
                (compileValuePush [] (AST.Integer 42))
            )
        , TestCase
            ( assertEqual
                "compile Float"
                (Right (VM.Push (VM.Float 3.14)))
                (compileValuePush [] (AST.Float 3.14))
            )
        , TestCase
            ( assertEqual
                "compile String"
                (Right (VM.Push (VM.String "Hello")))
                (compileValuePush [] (AST.String "Hello"))
            )
        , TestCase
            ( assertEqual
                "compile Identifier"
                (Right (VM.PushEnv "x"))
                (compileValuePush [] (AST.Identifier "x"))
            )
        , TestCase
            ( assertEqual
                "compile unknown type"
                (Left "Unknown value type")
                (compileValuePush [] (AST.Function "foo" [] []))
            )
        ]

testGetBuiltinCallForOp :: Test
testGetBuiltinCallForOp =
    TestList
        [ TestCase
            ( assertEqual
                "get plus operator"
                (Right VM.Add)
                (getBuiltinCallForOp "+")
            )
        , TestCase
            ( assertEqual
                "get minus operator"
                (Right VM.Sub)
                (getBuiltinCallForOp "-")
            )
        , TestCase
            ( assertEqual
                "get multiplication operator"
                (Right VM.Mul)
                (getBuiltinCallForOp "*")
            )
        , TestCase
            ( assertEqual
                "get division operator"
                (Right VM.Div)
                (getBuiltinCallForOp "/")
            )
        , TestCase
            ( assertEqual
                "get modulus operator"
                (Right VM.Mod)
                (getBuiltinCallForOp "%")
            )
        , TestCase
            ( assertEqual
                "get power operator"
                (Right VM.Pow)
                (getBuiltinCallForOp "**")
            )
        , TestCase
            ( assertEqual
                "get equal operator"
                (Right VM.Eq)
                (getBuiltinCallForOp "==")
            )
        , TestCase
            ( assertEqual
                "get not equal operator"
                (Right VM.Neq)
                (getBuiltinCallForOp "!=")
            )
        , TestCase
            ( assertEqual
                "get less than operator"
                (Right VM.Less)
                (getBuiltinCallForOp "<")
            )
        , TestCase
            ( assertEqual
                "get less than or equal operator"
                (Right VM.LessOrEqual)
                (getBuiltinCallForOp "<=")
            )
        , TestCase
            ( assertEqual
                "get greater than operator"
                (Right VM.Greater)
                (getBuiltinCallForOp ">")
            )
        , TestCase
            ( assertEqual
                "get greater than or equal operator"
                (Right VM.GreaterOrEqual)
                (getBuiltinCallForOp ">=")
            )
        , TestCase
            ( assertEqual
                "get logical and operator"
                (Right VM.And)
                (getBuiltinCallForOp "&&")
            )
        , TestCase
            ( assertEqual
                "get logical or operator"
                (Right VM.Or)
                (getBuiltinCallForOp "||")
            )
        , TestCase
            ( assertEqual
                "get xor operator"
                (Right VM.Xor)
                (getBuiltinCallForOp "^")
            )
        , TestCase
            ( assertEqual
                "get unknown operator"
                (Left "Unknown builtin call")
                (getBuiltinCallForOp "@")
            )
        ]

testCompileNode :: Test
testCompileNode =
    TestList
        [ TestCase
            ( assertEqual
                "compile Call node"
                (Right [VM.Push (VM.Integer 1), VM.PushEnv "function", VM.Call])
                (compileNode [] (AST.Call (AST.Identifier "function") [AST.Integer 1]))
            )
        , TestCase
            ( assertEqual
                "compile If node"
                (Right [VM.Push (VM.Bool True), VM.Jump 2 (Just False), VM.Push (VM.Integer 1), VM.Jump 1 Nothing, VM.Push (VM.Integer 0)])
                (compileNode [] (AST.If (AST.Boolean True) [AST.Integer 1] [AST.Integer 0]))
            )
        , TestCase
            ( assertEqual
                "compile Function node"
                (Right [VM.DefineEnv "myFunc" VM.Define (Just (VM.Function 1 [VM.PushArg 0, VM.DefineEnv "x" VM.Override Nothing, VM.PushEnv "x", VM.Ret]))])
                (compileNode [] (AST.Function "myFunc" ["x"] [AST.Return (Just (AST.Identifier "x"))]))
            )
        , TestCase
            ( assertEqual
                "compile Return node"
                (Right [VM.Push (VM.Integer 42), VM.Ret])
                (compileNode [] (AST.Return (Just (AST.Integer 42))))
            )
        , TestCase
            ( assertEqual
                "compile Integer value node"
                (Right [VM.Push (VM.Integer 42)])
                (compileNode [] (AST.Integer 42))
            )
        , TestCase
            ( assertEqual
                "compile unknown node type"
                (Left "Unknown node type")
                (compileNode [] (AST.Import "someModule"))
            )
        ]

testCompileReturn :: Test
testCompileReturn =
    TestList
        [ TestCase
            ( assertEqual
                "compile return with a call"
                (Right [VM.Push (VM.Integer 1), VM.PushEnv "function", VM.Call, VM.Ret])
                (compileReturn [] (Just (AST.Call (AST.Identifier "function") [AST.Integer 1])))
            )
        , TestCase
            ( assertEqual
                "compile return with a value"
                (Right [VM.Push (VM.Integer 42), VM.Ret])
                (compileReturn [] (Just (AST.Integer 42)))
            )
        , TestCase
            ( assertEqual
                "compile void return"
                (Right [VM.Push VM.Void, VM.Ret])
                (compileReturn [] Nothing)
            )
        ]

testCompileFunction :: Test
testCompileFunction =
    TestList
        [ TestCase
            ( assertEqual
                "compile simple function"
                (Right [VM.DefineEnv "myFunc" VM.Define (Just $ VM.Function 1 [VM.PushArg 0, VM.DefineEnv "x" VM.Override Nothing, VM.PushEnv "x", VM.Ret])])
                (compileFunction [] "myFunc" ["x"] [AST.Return (Just (AST.Identifier "x"))])
            )
        , TestCase
            ( assertEqual
                "compile function without parameters"
                (Right [VM.DefineEnv "noParamsFunc" VM.Define (Just $ VM.Function 0 [VM.Push (VM.Integer 1), VM.Ret])])
                (compileFunction [] "noParamsFunc" [] [AST.Return (Just (AST.Integer 1))])
            )
        , TestCase
            ( assertEqual
                "compile function with multiple parameters"
                (Right [VM.DefineEnv "multiParamsFunc" VM.Define (Just $ VM.Function 3 [VM.PushArg 0, VM.DefineEnv "x" VM.Override Nothing, VM.PushArg 1, VM.DefineEnv "y" VM.Override Nothing, VM.PushArg 2, VM.DefineEnv "z" VM.Override Nothing, VM.PushEnv "x", VM.Ret])])
                (compileFunction [] "multiParamsFunc" ["x", "y", "z"] [AST.Return (Just (AST.Identifier "x"))])
            )
        ]

testCompileLoop :: Test
testCompileLoop =
    TestList
        [ TestCase
            ( assertEqual
                "compile simple loop"
                (Right [VM.Push (VM.Bool True), VM.Jump 4 (Just False), VM.Push (VM.Integer 1), VM.PushEnv "print", VM.Call, VM.Jump (-6) Nothing])
                (compileLoop [] (AST.Boolean True) [AST.Call (AST.Identifier "print") [AST.Integer 1]] Nothing Nothing)
            )
        , TestCase
            ( assertEqual
                "compile loop with initialization"
                (Right [VM.Push (VM.Integer 0), VM.Push (VM.Bool True), VM.Jump 4 (Just False), VM.Push (VM.Integer 1), VM.PushEnv "print", VM.Call, VM.Jump (-6) Nothing])
                (compileLoop [] (AST.Boolean True) [AST.Call (AST.Identifier "print") [AST.Integer 1]] (Just (AST.Integer 0)) Nothing)
            )
        , TestCase
            ( assertEqual
                "compile loop with update"
                (Right [VM.Push (VM.Bool True), VM.Jump 6 (Just False), VM.Push (VM.Integer 1), VM.PushEnv "print", VM.Call, VM.PushEnv "i", VM.Call, VM.Jump (-8) Nothing])
                (compileLoop [] (AST.Boolean True) [AST.Call (AST.Identifier "print") [AST.Integer 1]] Nothing (Just (AST.Call (AST.Identifier "i") [])))
            )
        ]

testCompileCall :: Test
testCompileCall =
    TestList
        [ TestCase
            ( assertEqual
                "compile simple function call"
                (Right [VM.Push (VM.Integer 2), VM.Push (VM.Integer 1), VM.PushEnv "myFunc", VM.Call])
                (compileCall [] (AST.Identifier "myFunc") [AST.Integer 1, AST.Integer 2])
            )
        , TestCase
            ( assertEqual
                "compile simple assignment"
                (Right [VM.Push (VM.Integer 5), VM.DefineEnv "x" VM.Redefine Nothing])
                (compileCall [] (AST.Identifier "=") [AST.Identifier "x", AST.Integer 5])
            )
        , TestCase
            ( assertEqual
                "compile binary operation call"
                (Right [VM.Push (VM.Integer 3), VM.Push (VM.Integer 2), VM.Push (VM.Symbol (VM.Builtin VM.Add)), VM.Call])
                (compileCall [] (AST.Identifier "+") [AST.Integer 2, AST.Integer 3])
            )
        ]

testCompileIf :: Test
testCompileIf =
    TestList
        [ TestCase
            ( assertEqual
                "compile simple if"
                (Right [VM.Push (VM.Bool True), VM.Jump 2 (Just False), VM.Push (VM.Integer 1), VM.Jump 1 Nothing, VM.Push (VM.Integer 0)])
                (compileIf [] (AST.Boolean True) [AST.Integer 1] [AST.Integer 0])
            )
        , TestCase
            ( assertEqual
                "compile if with multiple statements"
                (Right [VM.Push (VM.Bool False), VM.Jump 5 (Just False), VM.PushEnv "func1", VM.Call, VM.PushEnv "func2", VM.Call, VM.Jump 2 Nothing, VM.PushEnv "func3", VM.Call])
                (compileIf [] (AST.Boolean False) [AST.Call (AST.Identifier "func1") [], AST.Call (AST.Identifier "func2") []] [AST.Call (AST.Identifier "func3") []])
            )
        , TestCase
            ( assertEqual
                "compile if without else"
                (Right [VM.Push (VM.Bool True), VM.Jump 1 (Just False), VM.Push (VM.Integer 1)])
                (compileIf [] (AST.Boolean True) [AST.Integer 1] [])
            )
        ]

testCompileBuiltinCall :: Test
testCompileBuiltinCall =
    TestList
        [ TestCase
            ( assertEqual
                "compile binary addition"
                (Right [VM.Push (VM.Integer 3), VM.Push (VM.Integer 2), VM.Push (VM.Symbol (VM.Builtin VM.Add)), VM.Call])
                (compileBuiltinCall [] "+" [AST.Integer 2, AST.Integer 3])
            )
        , TestCase
            ( assertEqual
                "compile binary subtraction"
                (Right [VM.Push (VM.Integer 2), VM.Push (VM.Integer 5), VM.Push (VM.Symbol (VM.Builtin VM.Sub)), VM.Call])
                (compileBuiltinCall [] "-" [AST.Integer 5, AST.Integer 2])
            )
        , TestCase
            ( assertEqual
                "compile binary multiplication"
                (Right [VM.Push (VM.Integer 6), VM.Push (VM.Integer 4), VM.Push (VM.Symbol (VM.Builtin VM.Mul)), VM.Call])
                (compileBuiltinCall [] "*" [AST.Integer 4, AST.Integer 6])
            )
        ]
