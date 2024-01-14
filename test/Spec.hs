import Control.Monad (unless)
import System.Exit (exitFailure)
import Test.HUnit (Test (..), errors, failures, runTestTT)
import Unit.Dreamberd.TestDreamberdBytecode (testDreamberdBytecode)
import Unit.Dreamberd.TestDreamberdParsing (testDreamberdParsing)
import Unit.Dreamberd.TestDreamberdTypes (testDreamberdTypes)
import Unit.Dreamberd.TestDreamberdVm (testDreamberdVm)
import Unit.Lisp.TestLispEval (testLispEvaluation)
import Unit.Lisp.TestLispParsing (testLispParsing)
import Unit.Lisp.TestLispSExpr (testLispSExpr)
import Unit.Dreamberd.TestDreamberdCompilation (testDreamberdCompilation)

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
        , testDreamberdBytecode
        , testDreamberdTypes
        , testDreamberdCompilation
        ]
