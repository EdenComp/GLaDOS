module Unit.Dreamberd.TestDreamberdCompilation where

import Test.HUnit (Test (..), assertEqual)

import Dreamberd.Vm as VM
import qualified Dreamberd.Types as AST


import Dreamberd.Compilation.Compile (compileAst, compileValuePush)

testDreamberdCompilation :: Test
testDreamberdCompilation =
    TestList [testCompileAst
        , testCompileValuePush
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

