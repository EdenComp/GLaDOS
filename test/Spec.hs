import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (Test (..), errors, failures, runTestTT)
import TestEval (testEvaluation)
import TestParsing (testParsing)
import TestSExpr (testSExpr)

main :: IO ()
main = do
    results <- runTestTT listTests
    if errors results + failures results == 0
        then exitSuccess
        else exitFailure

listTests :: Test
listTests =
    TestList
        [ testParsing
        , testSExpr
        , testEvaluation
        ]
