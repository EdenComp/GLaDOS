import Eval (testEvaluation)
import System.Exit (exitWith, ExitCode(..))
import Test.HUnit (Test(..), runTestTT, errors, failures)

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
    testEvaluation
    ]
