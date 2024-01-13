module Unit.Dreamberd.TestDreamberdTypes (testDreamberdTypes) where

import Dreamberd.Vm (Env (..))
import Test.HUnit (Test (..), assertEqual)

import qualified Dreamberd.Types as AST
import qualified Dreamberd.Vm as VM

testDreamberdTypes :: Test
testDreamberdTypes =
    TestList
        [ testAstTypes
        , testVmTypes
        , testVmTypesComparison
        ]

testAstTypes :: Test
testAstTypes =
    TestList
        [ TestCase (assertEqual "Number" (show (AST.Number 42)) "42")
        , TestCase (assertEqual "Float" (show (AST.Float 42.69)) "42.69")
        , TestCase (assertEqual "Boolean True" (show (AST.Boolean True)) "true")
        , TestCase (assertEqual "Boolean False" (show (AST.Boolean False)) "false")
        , TestCase (assertEqual "String" (show (AST.String "test")) "\"test\"")
        , TestCase (assertEqual "Identifier" (show (AST.Identifier "x")) "(Identifier x)")
        , TestCase (assertEqual "Function" (show (AST.Function "foo" [] [AST.Return (AST.Number 1)])) "Function \"foo\" [] [Return 1]")
        , TestCase (assertEqual "Call" (show (AST.Call "bar" [AST.Number 42])) "(Call bar [42])")
        , TestCase (assertEqual "Operator" (show (AST.Call "+" [AST.Number 2, AST.Number 3])) "(Call + [2,3])")
        , TestCase (assertEqual "AssignVariable" (show (AST.Call "=" [AST.Identifier "int", AST.Identifier "x", AST.Number 42])) "(Call = [(Identifier int),(Identifier x),42])")
        , TestCase
            ( assertEqual
                "If"
                (show (AST.If (AST.Boolean True) [AST.Return (AST.Number 1)] [AST.Return (AST.Number 2)]))
                "If (true) [Return 1] [Return 2]"
            )
        , TestCase (assertEqual "Return" (show (AST.Return (AST.String "hello"))) "Return \"hello\"")
        , TestCase (assertEqual "Loop" (show (AST.Loop (AST.Boolean True) [AST.Return (AST.Number 1)] Nothing Nothing)) "(Loop true [Return 1] Nothing Nothing)")
        , TestCase (assertEqual "List" (show (AST.List [AST.Number 1, AST.Number 2, AST.Number 3])) "[1 2 3]")
        ]

testVmTypes :: Test
testVmTypes =
    TestList
        [ TestCase (assertEqual "Number" (show (VM.Number 84)) "84")
        , TestCase (assertEqual "Float" (show (VM.Float 1.001)) "1.001")
        , TestCase (assertEqual "Boolean" (show (VM.Bool True)) "True")
        , TestCase (assertEqual "String" (show (VM.String "test")) "test")
        , TestCase (assertEqual "Symbol" (show (VM.Symbol (VM.FunctionName "foo"))) "FunctionName \"foo\"")
        , TestCase (assertEqual "Void" (show VM.Void) "")
        , TestCase (assertEqual "EnvValue" (show (VM.Variable (VM.String "test"))) "Variable test")
        , TestCase (assertEqual "Env" (show (VM.Env{identifier = "salut", value = VM.Variable (VM.String "hello")})) "Env {identifier = \"salut\", value = Variable hello}")
        , TestCase (assertEqual "Push" (show (VM.Push (VM.Number 42))) "Push 42")
        ]

testVmTypesComparison :: Test
testVmTypesComparison =
    TestList
        [ TestCase (assertEqual "Call equality" (VM.Builtin VM.Add == VM.Builtin VM.Add) True)
        , TestCase (assertEqual "Call inequality" (VM.Builtin VM.Sub == VM.Builtin VM.Mod) False)
        , TestCase (assertEqual "Call enum value" (fromEnum VM.Sub) 1)
        , TestCase (assertEqual "Call enum value 2" (toEnum 4) VM.Mod)
        ]
