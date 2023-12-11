module SExprTest (testSExpr) where

import Test.HUnit

import SExpr


testGetSymbol :: Test
testGetSymbol = TestList [
    TestCase (assertEqual "getSymbol (Symbol \"a\")" (Just "a") (getSymbol (Symbol "a"))),
    TestCase (assertEqual "getSymbol (Integer 1)" Nothing (getSymbol (Integer 1))),
    TestCase (assertEqual "getSymbol (Float 1.0)" Nothing (getSymbol (Float 1.0))),
    TestCase (assertEqual "getSymbol (List [])" Nothing (getSymbol (List []))),
    TestCase (assertEqual "getSymbol (List [Symbol \"a\"])" Nothing (getSymbol (List [Symbol "a"])))
    ]

testPrintTree :: Test
testPrintTree = TestList [
    TestCase (assertEqual "printTree (Symbol \"a\")" (Just "a Symbol 'a'") (printTree (Symbol "a"))),
    TestCase (assertEqual "printTree (Integer 1)" (Just "an Integer 1") (printTree (Integer 1))),
    TestCase (assertEqual "printTree (Float 1.0)" (Just "a Float 1.0") (printTree (Float 1.0))),
    TestCase (assertEqual "printTree (List [])" (Just "an empty List") (printTree (List []))),
    TestCase (assertEqual "printTree (List [Symbol \"a\"])" (Just "a List containing a Symbol 'a', followed by an empty List") (printTree (List [Symbol "a", List []])))
    ]


testSExpr :: Test
testSExpr = TestList [
    TestLabel "testGetSymbol" testGetSymbol,
    TestLabel "testPrintTree" testPrintTree
    ]