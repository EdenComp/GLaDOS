module Unit.Dreamberd.TestDreamberdCompilation where

import Test.HUnit (Test (..), assertEqual)

import Dreamberd.Vm as VM
import qualified Dreamberd.Types as AST


import Dreamberd.Compilation.Compile (compileAst, compileValuePush, getBuiltinCallForOp)

testDreamberdCompilation :: Test
testDreamberdCompilation =
    TestList [testCompileAst
        , testCompileValuePush
        , testGetBuiltinCallForOp
    ]

-- testCompileAst :: Test
testCompileAst =
    TestList
        []

testCompileValuePush :: Test
testCompileValuePush = TestList [
    TestCase (assertEqual "compile Boolean True"
        (Right (VM.Push (VM.Bool True)))
        (compileValuePush (AST.Boolean True))),

    TestCase (assertEqual "compile Boolean False"
        (Right (VM.Push (VM.Bool False)))
        (compileValuePush (AST.Boolean False))),

    TestCase (assertEqual "compile Integer"
        (Right (VM.Push (VM.Integer 42)))
        (compileValuePush (AST.Integer 42))),

    TestCase (assertEqual "compile Float"
        (Right (VM.Push (VM.Float 3.14)))
        (compileValuePush (AST.Float 3.14))),

    TestCase (assertEqual "compile String"
        (Right (VM.Push (VM.String "Hello")))
        (compileValuePush (AST.String "Hello"))),

    TestCase (assertEqual "compile Identifier"
        (Right (VM.PushEnv "x"))
        (compileValuePush (AST.Identifier "x"))),

    TestCase (assertEqual "compile unknown type"
        (Left "Unknown value type")
        (compileValuePush (AST.Function "foo" [] [])))
    ]

testGetBuiltinCallForOp :: Test
testGetBuiltinCallForOp = TestList [
    TestCase (assertEqual "get plus operator"
        (Right VM.Add)
        (getBuiltinCallForOp "+")),

    TestCase (assertEqual "get minus operator"
        (Right VM.Sub)
        (getBuiltinCallForOp "-")),

    TestCase (assertEqual "get multiplication operator"
        (Right VM.Mul)
        (getBuiltinCallForOp "*")),

    TestCase (assertEqual "get division operator"
        (Right VM.Div)
        (getBuiltinCallForOp "/")),

    TestCase (assertEqual "get modulus operator"
        (Right VM.Mod)
        (getBuiltinCallForOp "%")),

    TestCase (assertEqual "get power operator"
        (Right VM.Pow)
        (getBuiltinCallForOp "**")),

    TestCase (assertEqual "get equal operator"
        (Right VM.Eq)
        (getBuiltinCallForOp "==")),

    TestCase (assertEqual "get not equal operator"
        (Right VM.Neq)
        (getBuiltinCallForOp "!=")),

    TestCase (assertEqual "get less than operator"
        (Right VM.Less)
        (getBuiltinCallForOp "<")),

    TestCase (assertEqual "get less than or equal operator"
        (Right VM.LessOrEqual)
        (getBuiltinCallForOp "<=")),

    TestCase (assertEqual "get greater than operator"
        (Right VM.Greater)
        (getBuiltinCallForOp ">")),

    TestCase (assertEqual "get greater than or equal operator"
        (Right VM.GreaterOrEqual)
        (getBuiltinCallForOp ">=")),

    TestCase (assertEqual "get logical and operator"
        (Right VM.And)
        (getBuiltinCallForOp "&&")),

    TestCase (assertEqual "get logical or operator"
        (Right VM.Or)
        (getBuiltinCallForOp "||")),

    TestCase (assertEqual "get xor operator"
        (Right VM.Xor)
        (getBuiltinCallForOp "^")),

    TestCase (assertEqual "get unknown operator"
        (Left "Unknown builtin call")
        (getBuiltinCallForOp "@"))
    ]


