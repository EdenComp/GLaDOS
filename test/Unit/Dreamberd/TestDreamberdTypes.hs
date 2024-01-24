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
        [ TestCase (assertEqual "Number" (show (AST.Integer 42)) "42")
        , TestCase (assertEqual "Float" (show (AST.Float 42.69)) "42.69")
        , TestCase (assertEqual "Boolean True" (show (AST.Boolean True)) "true")
        , TestCase (assertEqual "Boolean False" (show (AST.Boolean False)) "false")
        , TestCase (assertEqual "String" (show (AST.String "test")) "\"test\"")
        , TestCase (assertEqual "Identifier" (show (AST.Identifier "x")) "(Identifier x)")
        , TestCase (assertEqual "Function" (show (AST.Function "foo" [] [AST.Return $ Just $ AST.Integer 1])) "Function \"foo\" [] [Return Just 1]")
        , TestCase (assertEqual "Call" (show (AST.Call (AST.Identifier "bar") [AST.Integer 42])) "(Call (Identifier bar) [42])")
        , TestCase (assertEqual "Operator" (show (AST.Call (AST.Identifier "+") [AST.Integer 2, AST.Integer 3])) "(Call (Identifier +) [2,3])")
        , TestCase (assertEqual "AssignVariable" (show (AST.Call (AST.Identifier "=") [AST.Identifier "int", AST.Identifier "x", AST.Integer 42])) "(Call (Identifier =) [(Identifier int),(Identifier x),42])")
        , TestCase
            ( assertEqual
                "If"
                (show (AST.If (AST.Boolean True) [AST.Return $ Just $ AST.Integer 1] [AST.Return $ Just $ AST.Integer 2]))
                "If (true) [Return Just 1] [Return Just 2]"
            )
        , TestCase (assertEqual "Return" (show (AST.Return $ Just $ AST.String "hello")) "Return Just \"hello\"")
        , TestCase (assertEqual "Loop" (show (AST.Loop (AST.Boolean True) [AST.Return $ Just $ AST.Integer 1] Nothing Nothing)) "(Loop true [Return Just 1] Nothing Nothing)")
        , TestCase (assertEqual "List" (show (AST.Scope [AST.Integer 1, AST.Integer 2, AST.Integer 3])) "{1 2 3}")
        ]

testVmTypes :: Test
testVmTypes =
    TestList
        [ TestCase (assertEqual "Number" (show (AST.Integer 84)) "84")
        , TestCase (assertEqual "Float" (show (VM.Float 1.001)) "1.001")
        , TestCase (assertEqual "Boolean" (show (VM.Bool True)) "True")
        , TestCase (assertEqual "String" (show (VM.String "test")) "test")
        , TestCase (assertEqual "Symbol" (show (VM.Symbol (VM.Builtin VM.Print))) "Builtin Print")
        , TestCase (assertEqual "Void" (show VM.Void) "")
        , TestCase (assertEqual "EnvValue" (show (VM.Variable (VM.String "test") 3)) "Variable test 3")
        , TestCase (assertEqual "DefineEnvType" (show VM.Redefine) "Redefine")
        , TestCase (assertEqual "Env" (show (VM.Env{identifier = "salut", value = VM.Variable (VM.String "hello") 1})) "Env {identifier = \"salut\", value = Variable hello 1}")
        , TestCase (assertEqual "Push" (show (VM.Push (VM.Integer 42))) "Push 42")
        ]

testVmTypesComparison :: Test
testVmTypesComparison =
    TestList
        [ TestCase (assertEqual "Call equality" (VM.Operator VM.Add == VM.Operator VM.Add) True)
        , TestCase (assertEqual "Call inequality" (VM.Operator VM.Sub == VM.Operator VM.Mod) False)
        , TestCase (assertEqual "Call enum value" (fromEnum VM.Sub) 1)
        , TestCase (assertEqual "Call enum value 2" (toEnum 4) VM.Mod)
        ]
