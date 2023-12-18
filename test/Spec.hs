import System.Exit (exitWith, ExitCode(..))
import TestEval (testEvaluation)
import Test.HUnit (Test(..), runTestTT, errors, failures)
import TestParsing (testParsing)
import TestSExpr (testSExpr)

main :: IO ()
main = do
    results <- runTestTT listTests
    if (errors results + failures results == 0)
        then
            exitWith ExitSuccess
        else
            exitWith (ExitFailure 1)

listTests :: Test
listTests = TestList [
    testParsing,
    testSExpr,
    testEvaluation
    ]
