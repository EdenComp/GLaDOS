module Unit.Dreamberd.TestDreamberdVm (testDreamberdVm) where

import Dreamberd.Vm (Call (..), Env (..), EnvValue (..), Insts (..), Value (..), exec)
import Test.HUnit (Test (..), assertEqual)

execVM :: [Insts] -> IO (Either String Value)
execVM = exec [] [] []

testDreamberdVm :: Test
testDreamberdVm =
    TestList
        [ testBasicExecution
        , testStackPushes
        , testCalls
        , testEnvDefines
        , testJumps
        , testFunctions
        ]

testBasicExecution :: Test
testBasicExecution =
    TestList
        [ TestCase $ execVM [] >>= assertEqual "empty" (Right Void)
        , TestCase $ execVM [Ret] >>= assertEqual "only return" (Right Void)
        , TestCase $ execVM [Push (Number 1)] >>= assertEqual "basic push" (Right Void)
        , TestCase $ execVM [Push (Number 1), Ret] >>= assertEqual "basic push with ret" (Right (Number 1))
        , TestCase $ execVM [Push (Number 1), Push (Number 2), Push (Symbol Add), Call, Ret] >>= assertEqual "basic operation" (Right (Number 3))
        , TestCase $ execVM [Push (Number 1), Push (Number 2), Push (Symbol Neq), Call, JumpIfFalse 2, Push (Bool False), Ret, Push (Bool True), Ret] >>= assertEqual "basic if true" (Right (Bool False))
        , TestCase $ execVM [Push (Number 1), Push (Number 2), Push (Symbol Eq), Call, JumpIfFalse 2, Push (Bool False), Ret, Push (Bool True), Ret] >>= assertEqual "basic if false" (Right (Bool True))
        ]

testStackPushes :: Test
testStackPushes =
    TestList
        [ TestCase $ execVM [PushArg 0] >>= assertEqual "PushArg without args" (Left "Argument index out of bounds")
        , TestCase $ exec [] [Number 1] [] [PushArg 0, Ret] >>= assertEqual "push from arg" (Right (Number 1))
        , TestCase $ exec [] [Number 1] [] [PushArg (-1), Ret] >>= assertEqual "push negative" (Left "Argument index out of bounds")
        , TestCase $ exec [Env{identifier = "ret", value = Function [PushArg 0, Ret]}] [] [] [Push (Number 10), PushEnv "ret", Call, Ret] >>= assertEqual "push arg from function" (Right (Number 10))
        , TestCase $ exec [Env{identifier = "ret", value = Function [PushArg 0, Ret]}] [] [] [Push (Number 10), PushEnv "res", Call, Ret] >>= assertEqual "push unknown env" (Left "Environment res does not exist")
        , TestCase $ exec [Env{identifier = "const", value = Variable (Bool True)}] [] [] [Push (Number 10), PushEnv "const", Ret] >>= assertEqual "push constant env" (Right (Bool True))
        ]

testCalls :: Test
testCalls =
    TestList
        [ TestCase $ execVM [Push (Number 0), Call] >>= assertEqual "not a symbol" (Left "Stack argument is not a symbol")
        , TestCase $ execVM [Call] >>= assertEqual "empty stack" (Left "Stack is empty for a Call instruction")
        , TestCase $ execVM [Push (Symbol (FunctionName "test")), Call] >>= assertEqual "unknown function" (Left "Environment test does not exist")
        , TestCase $ execVM [Push (Number 2), Push (Number 6), Push (Symbol Sub), Call, Push (Number 20), Push (Symbol Div), Call, Ret] >>= assertEqual "mixed operations" (Right (Number 5))
        , TestCase $ execVM [Push (Number 2), Push (Number 0), Push (Symbol Mul), Call, Push (Number 1), Push (Symbol Div), Call] >>= assertEqual "divide by zero" (Left "Cannot divide by 0")
        , TestCase $ execVM [Push (Number 1), Push (Number 2), Push (Number 0), Push (Symbol Mul), Call, Push (Symbol Div), Call, Ret] >>= assertEqual "divide zero" (Right (Number 0))
        ]

testEnvDefines :: Test
testEnvDefines =
    TestList
        [ TestCase $ execVM [Push (Number (-26)), DefineEnv "opp" (Function [PushArg 0, Push (Number (-1)), Push (Symbol Mul), Call, Ret]), PushEnv "opp", Call, Ret] >>= assertEqual "basic function" (Right (Number 26))
        , TestCase $ execVM [DefineEnv "inc" (Function [PushArg 0, DefineEnv "val" (Variable (Number 2)), PushEnv "val", Push (Symbol Add), Call, Ret]), Push (Number 2), PushEnv "inc", Call, Ret] >>= assertEqual "define inside define" (Right (Number 4))
        , TestCase $ execVM [DefineEnv "inc" (Function [PushArg 0, DefineEnv "val" (Variable (Number 2)), PushEnv "val", Push (Symbol Add), Call, Ret]), PushEnv "val", Call, Ret] >>= assertEqual "private scopes" (Left "Environment val does not exist")
        , TestCase $ execVM [DefineEnv "idx" (Variable (Number 3)), DefineEnv "mul" (Function [PushEnv "idx", PushArg 0, Push (Symbol Mul), Call, Ret]), Push (String "hey"), PushEnv "mul", Call, Ret] >>= assertEqual "parent scope" (Right (String "heyheyhey"))
        , TestCase $ execVM [DefineEnv "test" (Function [Push (Bool True), Push (Symbol Eq), Call, Ret]), PushEnv "test", Call, Ret] >>= assertEqual "error inside function" (Left "Wrong stack variables for builtin Eq")
        , TestCase $ execVM [DefineEnv "begin" (Variable (String "bonjour")), DefineEnv "begin" (Variable (String "hello")), Push (String " world"), PushEnv "begin", Push (Symbol Add), Call, Ret] >>= assertEqual "environment reassignment" (Right (String "hello world"))
        , TestCase $ execVM [Push (String "Hello World"), DefineEnvFromStack "message", Ret] >>= assertEqual "basic DefineEnvFromStack" (Right Void)
        , TestCase $ execVM [DefineEnvFromStack "message", Ret] >>= assertEqual "empty stack for DefineEnvFromStack" (Left "Stack is empty for a DefineEnvFromStack \"message\" instruction")
        , TestCase $ execVM [Push (Number 1), Push (Number 2), Push (Bool True), DefineEnvFromStack "fst", DefineEnvFromStack "sec", DefineEnvFromStack "neq", PushEnv "sec", Ret] >>= assertEqual "wipe stack" (Right (Number 2))
        ]

testJumps :: Test
testJumps =
    TestList
        [ TestCase $ execVM [Push (Bool True), JumpIfFalse 2, Push (Number 1), Ret, Push (Number 2), Ret] >>= assertEqual "basic jump" (Right (Number 1))
        , TestCase $ execVM [JumpIfFalse 1, Push (Number 1), Ret, Push (Number 2), Ret] >>= assertEqual "jump with empty stack" (Left "Stack is empty for a JumpIfFalse 1 instruction")
        , TestCase $ execVM [Push (Number 2), JumpIfFalse 1, Push (Number 1), Ret, Push (Number 2), Ret] >>= assertEqual "jump with a number" (Left "Wrong data types in stack: JumpIfFalse needs a Bool")
        , TestCase $ execVM [Push (Bool False), JumpIfFalse (-1), Push (Number 1), Ret, Push (Number 2), Ret] >>= assertEqual "negative jump" (Left "Invalid number of instructions")
        , TestCase $ execVM [Push (Bool True), JumpIfFalse 5, Push (Number 1), Ret, Push (Number 2), Ret] >>= assertEqual "too long jump" (Left "Invalid number of instructions")
        ]

testFunctions :: Test
testFunctions =
    TestList
        [ TestCase $ execVM [DefineEnv "fact" (Function [PushArg 0, Push (Number 1), Push (Symbol Eq), Call, JumpIfFalse 2, Push (Number 1), Ret, Push (Number 1), PushArg 0, Push (Symbol Sub), Call, PushEnv "fact", Call, PushArg 0, Push (Symbol Mul), Call, Ret]), Push (Number 5), PushEnv "fact", Call, Ret] >>= assertEqual "factorial" (Right (Number 120))
        ]
