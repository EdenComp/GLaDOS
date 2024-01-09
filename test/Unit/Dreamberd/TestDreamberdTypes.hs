module Unit.Dreamberd.TestDreamberdTypes (testDreamberdTypes) where

import Dreamberd.Types
import Test.HUnit (Test (..), assertEqual)

testDreamberdTypes :: Test
testDreamberdTypes =
  TestList
    [ testNumber,
      testBoolean,
      testString,
      testIdentifier,
      testFunction,
      testCall,
      testOperator,
      testAssignVariable,
      testIf,
      testReturn,
      testLoop,
      testList
    ]

testNumber :: Test
testNumber =
  TestList
    [ TestCase (assertEqual "Number" (show (Number 42)) "42")
    ]

testBoolean :: Test
testBoolean =
  TestList
    [ TestCase (assertEqual "Boolean True" (show (Boolean True)) "true"),
      TestCase (assertEqual "Boolean False" (show (Boolean False)) "false")
    ]

testString :: Test
testString =
  TestList
    [ TestCase (assertEqual "String" (show (String "test")) "\"test\"")
    ]

testIdentifier :: Test
testIdentifier =
  TestList
    [ TestCase (assertEqual "Identifier" (show (Identifier "x")) "(Identifier x)")
    ]

testFunction :: Test
testFunction =
  TestList
    [ TestCase (assertEqual "Function" (show (Function "foo" [] [Return (Number 1)])) "Function \"foo\" [] [Return 1]")
    ]

testCall :: Test
testCall =
  TestList
    [ TestCase (assertEqual "Call" (show (Call "bar" [Number 42])) "(Call bar [42])")
    ]

testOperator :: Test
testOperator =
  TestList
    [ TestCase (assertEqual "Operator" (show (Operator "+" (Number 2) (Number 3))) "(Operator \"+\" 2 3)")
    ]

testAssignVariable :: Test
testAssignVariable =
  TestList
    [ TestCase (assertEqual "AssignVariable" (show (AssignVariable "int" "x" (Number 42))) "(AssignVariable int \"x\" 42)")
    ]

testIf :: Test
testIf =
  TestList
    [ TestCase
        ( assertEqual
            "If"
            (show (If (Boolean True) [Return (Number 1)] [Return (Number 2)]))
            "If (true) [Return 1] [Return 2]"
        )
    ]

testReturn :: Test
testReturn =
  TestList
    [ TestCase (assertEqual "Return" (show (Return (String "hello"))) "Return \"hello\"")
    ]
testLoop :: Test
testLoop =
  TestList
    [ TestCase (assertEqual "Loop" (show (Loop (Boolean True) [Return (Number 1)] Nothing Nothing)) "(Loop true [Return 1] Nothing Nothing)")
    ]

testList :: Test
testList =
  TestList
    [ TestCase (assertEqual "List" (show (List [Number 1, Number 2, Number 3])) "[1 2 3]")
    ]
