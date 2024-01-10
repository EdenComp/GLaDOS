module Unit.Dreamberd.TestDreamberdTypes (testDreamberdTypes) where

import Dreamberd.Types
import Test.HUnit (Test (..), assertEqual)

testDreamberdTypes :: Test
testDreamberdTypes =
    TestList
        [ TestCase (assertEqual "Number" (show (Number 42)) "42")
        , TestCase (assertEqual "Boolean True" (show (Boolean True)) "true")
        , TestCase (assertEqual "Boolean False" (show (Boolean False)) "false")
        , TestCase (assertEqual "String" (show (String "test")) "\"test\"")
        , TestCase (assertEqual "Identifier" (show (Identifier "x")) "(Identifier x)")
        , TestCase (assertEqual "Function" (show (Function "foo" [] [Return (Number 1)])) "Function \"foo\" [] [Return 1]")
        , TestCase (assertEqual "Call" (show (Call "bar" [Number 42])) "(Call bar [42])")
        , TestCase (assertEqual "Operator" (show (Call "+" [Number 2, Number 3])) "(Call + [2,3])")
        , TestCase (assertEqual "AssignVariable" (show (Call "=" [Identifier "int", Identifier "x", Number 42])) "(Call = [(Identifier int),(Identifier x),42])")
        , TestCase
            ( assertEqual
                "If"
                (show (If (Boolean True) [Return (Number 1)] [Return (Number 2)]))
                "If (true) [Return 1] [Return 2]"
            )
        , TestCase (assertEqual "Return" (show (Return (String "hello"))) "Return \"hello\"")
        , TestCase (assertEqual "Loop" (show (Loop (Boolean True) [Return (Number 1)] Nothing Nothing)) "(Loop true [Return 1] Nothing Nothing)")
        , TestCase (assertEqual "List" (show (List [Number 1, Number 2, Number 3])) "[1 2 3]")
        ]
