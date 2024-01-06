import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (Test (..), errors, failures, runTestTT)
import Unit.Dreamberd.TestDreamberdParsing (testDreamberdParsing)
import Unit.Lisp.TestLispEval (testLispEvaluation)
import Unit.Lisp.TestLispParsing (testLispParsing)
import Unit.Lisp.TestLispSExpr (testLispSExpr)

main :: IO ()
main = do
    results <- runTestTT listTests
    if errors results + failures results == 0
        then exitSuccess
        else exitFailure

listTests :: Test
listTests =
    TestList
        [ testLispParsing
        , testLispSExpr
        , testLispEvaluation
        , testDreamberdParsing
        ]
