module Unit.Dreamberd.TestDreamberdVm (testDreamberdVm) where

import Dreamberd.Vm (Call (..), Env (..), EnvValue (..), Insts (..), Operator (..), Value (..), exec, execVM)
import Test.HUnit (Test (..), assertEqual)

testDreamberdVm :: Test
testDreamberdVm =
    TestList
        [ testBasicExecution
        , testStackPushes
        , testCalls
        , testBuiltins
        , testEnvDefinesAndRedefines
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
        , TestCase $ execVM [Push (Number 1), Push (Number 2), Push (Symbol (Builtin Add)), Call, Ret] >>= assertEqual "basic operation" (Right (Number 3))
        , TestCase $ execVM [Push (Number 1), Push (Number 2), Push (Symbol (Builtin Neq)), Call, Jump 2 (Just False), Push (Bool False), Ret, Push (Bool True), Ret] >>= assertEqual "basic if true" (Right (Bool False))
        , TestCase $ execVM [Push (Number 1), Push (Number 2), Push (Symbol (Builtin Eq)), Call, Jump 2 (Just False), Push (Bool False), Ret, Push (Bool True), Ret] >>= assertEqual "basic if false" (Right (Bool True))
        , TestCase $ exec [] [] [] [Push (Number 0), Ret] (-1) >>= assertEqual "wrong index: negative" (Left "Instructions index out of bounds")
        , TestCase $ exec [] [] [] [Push (Number 0), Ret] 3 >>= assertEqual "wrong index: too long" (Left "Instructions index out of bounds")
        ]

testStackPushes :: Test
testStackPushes =
    TestList
        [ TestCase $ execVM [PushArg 0] >>= assertEqual "PushArg without args" (Left "Argument index out of bounds")
        , TestCase $ exec [] [Number 1] [] [PushArg 0, Ret] 0 >>= assertEqual "push from arg" (Right (Number 1))
        , TestCase $ exec [] [Number 1] [] [PushArg (-1), Ret] 0 >>= assertEqual "push negative" (Left "Argument index out of bounds")
        , TestCase $ exec [Env{identifier = "ret", value = Function [PushArg 0, Ret]}] [] [] [Push (Number 10), PushEnv "ret", Call, Ret] 0 >>= assertEqual "push arg from function" (Right (Number 10))
        , TestCase $ exec [Env{identifier = "ret", value = Function [PushArg 0, Ret]}] [] [] [Push (Number 10), PushEnv "res", Call, Ret] 0 >>= assertEqual "push unknown env" (Left "Environment res does not exist")
        , TestCase $ exec [Env{identifier = "const", value = Variable (Bool True)}] [] [] [Push (Number 10), PushEnv "const", Ret] 0 >>= assertEqual "push constant env" (Right (Bool True))
        ]

testCalls :: Test
testCalls =
    TestList
        [ TestCase $ execVM [Push (Number 0), Call] >>= assertEqual "not a symbol" (Left "Stack argument is not a symbol")
        , TestCase $ execVM [Call] >>= assertEqual "empty stack" (Left "Stack is empty for a Call instruction")
        , TestCase $ execVM [Push (Symbol (FunctionName "test")), Call] >>= assertEqual "unknown function" (Left "Environment test does not exist")
        , TestCase $ execVM [Push (Number 2), Push (Number 2), Push (Number 6), Push (Symbol (Builtin Sub)), Call, Push (Number 20), Push (Symbol (Builtin Div)), Call, Push (Symbol (Builtin Mod)), Call, Ret] >>= assertEqual "mixed operations" (Right (Number 1))
        , TestCase $ execVM [Push (Number 2), Push (Number 0), Push (Symbol (Builtin Mul)), Call, Push (Number 1), Push (Symbol (Builtin Div)), Call] >>= assertEqual "divide by zero" (Left "Cannot divide by 0")
        , TestCase $ execVM [Push (Number 0), Push (Number 0), Push (Symbol (Builtin Mod)), Call, Ret] >>= assertEqual "mod by zero" (Left "Cannot divide by 0")
        , TestCase $ execVM [Push (Number 1), Push (Number 2), Push (Number 0), Push (Symbol (Builtin Div)), Call, Push (Symbol (Builtin Div)), Call, Ret] >>= assertEqual "divide zero" (Right (Number 0))
        ]

testBuiltins :: Test
testBuiltins =
    TestList
        [ TestCase $ execVM [Push (Float 1.44), Push (Float 2.56), Push (Symbol (Builtin Add)), Call, Ret] >>= assertEqual "floats: add" (Right (Float 4.0))
        , TestCase $ execVM [Push (Float 10.8), Push (Float 1.5), Push (Symbol (Builtin Sub)), Call, Ret] >>= assertEqual "floats: sub" (Right (Float (-9.3)))
        , TestCase $ execVM [Push (Float 2.5), Push (Float 2.5), Push (Symbol (Builtin Mul)), Call, Ret] >>= assertEqual "floats: mul" (Right (Float 6.25))
        , TestCase $ execVM [Push (Float 1.4), Push (Float 5.6), Push (Symbol (Builtin Div)), Call, Ret] >>= assertEqual "floats: div" (Right (Float 4.0))
        , TestCase $ execVM [Push (Float 1.5), Push (Float 5.5), Push (Symbol (Builtin Mod)), Call, Ret] >>= assertEqual "floats: mod" (Right (Float 1.0))
        , TestCase $ execVM [Push (Float 1.5), Push (Float 5.5), Push (Symbol (Builtin Less)), Call, Ret] >>= assertEqual "floats: less" (Right (Bool False))
        , TestCase $ execVM [Push (Float 1.5), Push (Float 5.5), Push (Symbol (Builtin LessOrEqual)), Call, Ret] >>= assertEqual "floats: less or equal" (Right (Bool False))
        , TestCase $ execVM [Push (Float 1.5), Push (Float 5.5), Push (Symbol (Builtin Greater)), Call, Ret] >>= assertEqual "floats: greater" (Right (Bool True))
        , TestCase $ execVM [Push (Float 1.5), Push (Float 1.5), Push (Symbol (Builtin GreaterOrEqual)), Call, Ret] >>= assertEqual "floats: greater or equal" (Right (Bool True))
        , TestCase $ execVM [Push (Float 1.5), Push (Float 5.5), Push (Symbol (Builtin Eq)), Call, Ret] >>= assertEqual "floats: eq" (Right (Bool False))
        , TestCase $ execVM [Push (Float 1.5), Push (Float 5.5), Push (Symbol (Builtin Neq)), Call, Ret] >>= assertEqual "floats: neq" (Right (Bool True))
        , TestCase $ execVM [Push (Number 1), Push (Number (-1)), Push (Symbol (Builtin Less)), Call, Ret] >>= assertEqual "numbers: less" (Right (Bool True))
        , TestCase $ execVM [Push (Number 0), Push (Number 1), Push (Symbol (Builtin LessOrEqual)), Call, Ret] >>= assertEqual "numbers: less or equal" (Right (Bool False))
        , TestCase $ execVM [Push (Number 5), Push (Number 15), Push (Symbol (Builtin Greater)), Call, Ret] >>= assertEqual "numbers: greater" (Right (Bool True))
        , TestCase $ execVM [Push (Number 18), Push (String "the last number is... "), Push (Symbol (Builtin Add)), Call, Ret] >>= assertEqual "strings: concat to number" (Right (String "the last number is... 18"))
        , TestCase $ execVM [Push (Bool False), Push (Bool True), Push (Symbol (Builtin Eq)), Call, Ret] >>= assertEqual "bools: eq" (Right (Bool False))
        , TestCase $ execVM [Push (Bool False), Push (Bool True), Push (Symbol (Builtin Neq)), Call, Ret] >>= assertEqual "bools: neq" (Right (Bool True))
        , TestCase $ execVM [Push (String "hello"), Push (String "hello"), Push (Symbol (Builtin Eq)), Call, Ret] >>= assertEqual "strings: eq" (Right (Bool True))
        , TestCase $ execVM [Push (String "hello"), Push (String "hello"), Push (Symbol (Builtin Neq)), Call, Ret] >>= assertEqual "strings: neq" (Right (Bool False))
        , TestCase $ execVM [Push (Float 0), Push (Float 1), Push (Symbol (Builtin Div)), Call, Ret] >>= assertEqual "floats: div by zero" (Left "Cannot divide by 0")
        , TestCase $ execVM [Push (Float 0), Push (Float 1), Push (Symbol (Builtin Mod)), Call, Ret] >>= assertEqual "floats: mod by zero" (Left "Cannot divide by 0")
        , TestCase $ execVM [Push (Bool True), Push (Bool False), Push (Symbol (Builtin Less)), Call, Ret] >>= assertEqual "bools: incompatible operator" (Left "Wrong data types in stack: Less with 2 booleans")
        , TestCase $ execVM [Push (String "hey"), Push (String "yo"), Push (Symbol (Builtin GreaterOrEqual)), Call, Ret] >>= assertEqual "strings: incompatible operator" (Left "Wrong data types in stack: GreaterOrEqual with a string as left operator")
        , TestCase $ execVM [Push (Number 2), Push (String "yo"), Push (Symbol (Builtin Sub)), Call, Ret] >>= assertEqual "string and number: incompatible operator" (Left "Wrong data types in stack: Sub with a string and a number")
        ]

testEnvDefinesAndRedefines :: Test
testEnvDefinesAndRedefines =
    TestList
        [ TestCase $ execVM [Push (Number (-26)), DefineEnv "opp" False (Just (Function [PushArg 0, Push (Number (-1)), Push (Symbol (Builtin Mul)), Call, Ret])), PushEnv "opp", Call, Ret] >>= assertEqual "basic function" (Right (Number 26))
        , TestCase $ execVM [DefineEnv "inc" False (Just (Function [PushArg 0, DefineEnv "val" False (Just (Variable (Number 2))), PushEnv "val", Push (Symbol (Builtin Add)), Call, Ret])), Push (Number 2), PushEnv "inc", Call, Ret] >>= assertEqual "define inside define" (Right (Number 4))
        , TestCase $ execVM [DefineEnv "inc" False (Just (Function [PushArg 0, DefineEnv "val" False (Just (Variable (Number 2))), PushEnv "val", Push (Symbol (Builtin Add)), Call, Ret])), PushEnv "val", Call, Ret] >>= assertEqual "private scopes" (Left "Environment val does not exist")
        , TestCase $ execVM [DefineEnv "idx" False (Just (Variable (Number 3))), DefineEnv "mul" False (Just (Function [PushEnv "idx", PushArg 0, Push (Symbol (Builtin Mul)), Call, Ret])), Push (String "hey"), PushEnv "mul", Call, Ret] >>= assertEqual "parent scope" (Right (String "heyheyhey"))
        , TestCase $ execVM [DefineEnv "test" False (Just (Function [Push (Bool True), Push (Symbol (Builtin Eq)), Call, Ret])), PushEnv "test", Call, Ret] >>= assertEqual "error inside function" (Left "Wrong stack variables for builtin Eq")
        , TestCase $ execVM [DefineEnv "begin" False (Just (Variable (String "bonjour"))), DefineEnv "begin" True (Just (Variable (String "hello"))), Push (String " world"), PushEnv "begin", Push (Symbol (Builtin Add)), Call, Ret] >>= assertEqual "environment reassignment" (Right (String "hello world"))
        , TestCase $ execVM [Push (String "Hello World"), DefineEnv "message" False Nothing, Ret] >>= assertEqual "basic DefineEnv from stack" (Right Void)
        , TestCase $ execVM [DefineEnv "message" False Nothing, Ret] >>= assertEqual "empty stack for DefineEnv from stack" (Left "Stack is empty for a DefineEnv from stack instruction")
        , TestCase $ execVM [Push (Number 1), Push (Number 2), Push (Bool True), DefineEnv "fst" False Nothing, DefineEnv "sec" False Nothing, DefineEnv "neq" False Nothing, PushEnv "sec", Ret] >>= assertEqual "wipe stack" (Right (Number 2))
        , TestCase $ execVM [DefineEnv "test" False (Just (Variable (Bool True))), EraseEnv "test", PushEnv "test", Ret] >>= assertEqual "use of erased env" (Left "Environment test does not exist")
        , TestCase $ execVM [DefineEnv "test" False (Just (Variable (Bool True))), EraseEnv "test", DefineEnv "test" False (Just (Variable (Bool False))), PushEnv "test", Ret] >>= assertEqual "define after erased env" (Right (Bool False))
        , TestCase $ execVM [DefineEnv "test" False (Just (Variable (Bool True))), EraseEnv "test", DefineEnv "test" True (Just (Variable (Bool False))), PushEnv "test", Ret] >>= assertEqual "redefine after erased env" (Left "Environment test does not exist")
        , TestCase $ execVM [DefineEnv "a" False (Just (Variable (Number 0))), DefineEnv "a" False (Just (Variable (String "hello"))), PushEnv "a", Ret] >>= assertEqual "double define" (Left "Environment a already exists")
        , TestCase $ execVM [DefineEnv "a" False (Just (Variable (Number 0))), Push (Bool False), DefineEnv "a" False Nothing, PushEnv "a", Ret] >>= assertEqual "double define from stack" (Left "Environment a already exists")
        , TestCase $ execVM [Push (Bool False), DefineEnv "a" True Nothing, PushEnv "a", Ret] >>= assertEqual "redefine from stack witout define" (Left "Environment a does not exist")
        , TestCase $ execVM [EraseEnv "test", DefineEnv "test" True Nothing, PushEnv "test", Ret] >>= assertEqual "erase before define" (Left "Environment test does not exist")
        ]

testJumps :: Test
testJumps =
    TestList
        [ TestCase $ execVM [Jump 1 Nothing, Ret, Push (Number 1), Ret] >>= assertEqual "basic jump" (Right (Number 1))
        , TestCase $ execVM [Push (Bool True), Jump 2 (Just False), Push (Number 1), Ret, Push (Number 2), Ret] >>= assertEqual "basic jump if false" (Right (Number 1))
        , TestCase $ execVM [Jump 1 (Just False), Push (Number 1), Ret, Push (Number 2), Ret] >>= assertEqual "jump with empty stack" (Left "Stack is empty for a conditional jump")
        , TestCase $ execVM [Push (Number 2), Jump 3 (Just False), Push (Number 1), Ret, Push (Number 2), Ret] >>= assertEqual "jump with a number" (Right (Number 1))
        , TestCase $ execVM [Push (Number 0), Jump 2 (Just False), Push (Number 1), Ret, Push (Number 2), Ret] >>= assertEqual "jump with a number" (Right (Number 2))
        , TestCase $ execVM [Push (String ""), Jump 5 Nothing, Push (Number 2), Push (Number 0), Push (Symbol (Builtin Div)), Call, Ret, Jump (-6) (Just False)] >>= assertEqual "jump with a negative number" (Right (Number 0))
        , TestCase $ execVM [Push (String ""), Jump 5 Nothing, Push (Number 2), Push (Number 0), Push (Symbol (Builtin Div)), Call, Ret, Jump (-9) Nothing] >>= assertEqual "too long negative jump" (Left "Invalid number of instructions")
        , TestCase $ execVM [Push (Bool False), Jump (-1) (Just False), Push (Number 1), Ret, Push (Number 2), Ret] >>= assertEqual "invalid jump" (Left "Invalid number of instructions")
        , TestCase $ execVM [Push (Bool True), Jump 5 (Just False), Push (Number 1), Ret, Push (Number 2), Ret] >>= assertEqual "too long jump" (Left "Invalid number of instructions")
        , TestCase $ execVM [Push Void, Jump 1 (Just True), Ret, Push (Number 1), Ret] >>= assertEqual "void as a value" (Right Void)
        ]

testFunctions :: Test
testFunctions =
    TestList
        [ TestCase $ execVM [DefineEnv "fact" False (Just (Function [PushArg 0, Push (Number 1), Push (Symbol (Builtin Eq)), Call, Jump 2 (Just False), Push (Number 1), Ret, Push (Number 1), PushArg 0, Push (Symbol (Builtin Sub)), Call, PushEnv "fact", Call, PushArg 0, Push (Symbol (Builtin Mul)), Call, Ret])), Push (Number 5), PushEnv "fact", Call, Ret] >>= assertEqual "factorial" (Right (Number 120))
        , TestCase $ execVM [DefineEnv "a" False (Just (Variable (Number 0))), DefineEnv "b" False (Just (Variable (String "hello"))), Push (Bool False), Jump 10 (Just False), Push (Number 1), PushEnv "a", Push (Symbol (Builtin Add)), Call, DefineEnv "a" True Nothing, Push (Number 2), PushEnv "b", Push (Symbol (Builtin Mul)), Call, DefineEnv "b" True Nothing, Push (Number 2), PushEnv "a", Push (Symbol (Builtin GreaterOrEqual)), Call, Jump (-15) (Just False), PushEnv "b", Ret] >>= assertEqual "for loop" (Right (String "hellohellohellohello"))
        , TestCase $ execVM [Push (Number 10), PushEnv "print", Call, Ret] >>= assertEqual "basic print" (Right Void)
        , TestCase $ execVM [PushEnv "print", Call, Ret] >>= assertEqual "print with empty stack" (Left "Stack is empty for print instruction")
        ]
