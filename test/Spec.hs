import Control.Monad (unless)
import System.Exit (exitFailure)
import Test.HUnit (Test (..), errors, failures, runTestTT)
import Unit.Dreamberd.TestDreamberdParsing (testDreamberdParsing)
import Unit.Dreamberd.TestDreamberdVm (testDreamberdVm)
import Unit.Lisp.TestLispEval (testLispEvaluation)
import Unit.Lisp.TestLispParsing (testLispParsing)
import Unit.Lisp.TestLispSExpr (testLispSExpr)

main :: IO ()
main = do
    results <- runTestTT listTests
    unless (errors results + failures results == 0) exitFailure

listTests :: Test
listTests =
    TestList
        [ testLispParsing
        , testLispSExpr
        , testLispEvaluation
        , testDreamberdParsing
        , testDreamberdVm
        ]
