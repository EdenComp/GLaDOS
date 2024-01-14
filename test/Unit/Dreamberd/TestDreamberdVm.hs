module Unit.Dreamberd.TestDreamberdVm (testDreamberdVm) where

import Test.HUnit (Test (..), assertEqual)

import Dreamberd.Vm as VM

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
        [ TestCase $ execVM [] >>= assertEqual "empty" (Right VM.Void)
        , TestCase $ execVM [Ret] >>= assertEqual "only return" (Right VM.Void)
        , TestCase $ execVM [Push (VM.Integer 1)] >>= assertEqual "basic push" (Right VM.Void)
        , TestCase $ execVM [Push (VM.Integer 1), Ret] >>= assertEqual "basic push with ret" (Right (VM.Integer 1))
        , TestCase $ execVM [Push (VM.Integer 1), Push (VM.Integer 2), Push (VM.Symbol (Builtin Add)), Call, Ret] >>= assertEqual "basic operation" (Right (VM.Integer 3))
        , TestCase $ execVM [Push (VM.Integer 1), Push (VM.Integer 2), Push (VM.Symbol (Builtin Neq)), Call, Jump 2 (Just False), Push (VM.Bool False), Ret, Push (VM.Bool True), Ret] >>= assertEqual "basic if true" (Right (VM.Bool False))
        , TestCase $ execVM [Push (VM.Integer 1), Push (VM.Integer 2), Push (VM.Symbol (Builtin Eq)), Call, Jump 2 (Just False), Push (VM.Bool False), Ret, Push (VM.Bool True), Ret] >>= assertEqual "basic if false" (Right (VM.Bool True))
        , TestCase $ exec [] [] [] [Push (VM.Integer 0), Ret] (-1) 0 >>= assertEqual "wrong index: negative" (Left "Instructions index out of bounds")
        , TestCase $ exec [] [] [] [Push (VM.Integer 0), Ret] 3 0 >>= assertEqual "wrong index: too long" (Left "Instructions index out of bounds")
        ]

testStackPushes :: Test
testStackPushes =
    TestList
        [ TestCase $ execVM [PushArg 0] >>= assertEqual "PushArg without args" (Left "Argument index out of bounds")
        , TestCase $ exec [] [VM.Integer 1] [] [PushArg 0, Ret] 0 0 >>= assertEqual "push from arg" (Right (VM.Integer 1))
        , TestCase $ exec [] [VM.Integer 1] [] [PushArg (-1), Ret] 0 0 >>= assertEqual "push negative" (Left "Argument index out of bounds")
        , TestCase $ exec [Env{identifier = "ret", value = Function [PushArg 0, Ret], scope = 1}] [] [] [Push (VM.Integer 10), PushEnv "ret", Call, Ret] 0 0 >>= assertEqual "push arg from function" (Right (VM.Integer 10))
        , TestCase $ exec [Env{identifier = "ret", value = Function [PushArg 0, Ret], scope = 1}] [] [] [Push (VM.Integer 10), PushEnv "res", Call, Ret] 0 0 >>= assertEqual "push unknown env" (Left "Environment res does not exist")
        , TestCase $ exec [Env{identifier = "const", value = Variable (VM.Bool True), scope = 1}] [] [] [Push (VM.Integer 10), PushEnv "const", Ret] 0 0 >>= assertEqual "push constant env" (Right (VM.Bool True))
        ]

testCalls :: Test
testCalls =
    TestList
        [ TestCase $ execVM [Push (VM.Integer 0), Call] >>= assertEqual "not a symbol" (Left "Stack argument is not a symbol")
        , TestCase $ execVM [Call] >>= assertEqual "empty stack" (Left "Stack is empty for a Call instruction")
        , TestCase $ execVM [Push (VM.Symbol (FunctionName "test")), Call] >>= assertEqual "unknown function" (Left "Environment test does not exist")
        , TestCase $ execVM [Push (VM.Integer 2), Push (VM.Integer 2), Push (VM.Integer 6), Push (VM.Symbol (Builtin Sub)), Call, Push (VM.Integer 20), Push (VM.Symbol (Builtin Div)), Call, Push (VM.Symbol (Builtin Mod)), Call, Ret] >>= assertEqual "mixed operations" (Right (VM.Integer 1))
        , TestCase $ execVM [Push (VM.Integer 2), Push (VM.Integer 0), Push (VM.Symbol (Builtin Mul)), Call, Push (VM.Integer 1), Push (VM.Symbol (Builtin Div)), Call] >>= assertEqual "divide by zero" (Left "Cannot divide by 0")
        , TestCase $ execVM [Push (VM.Integer 0), Push (VM.Integer 0), Push (VM.Symbol (Builtin Mod)), Call, Ret] >>= assertEqual "mod by zero" (Left "Cannot divide by 0")
        , TestCase $ execVM [Push (VM.Integer 1), Push (VM.Integer 2), Push (VM.Integer 0), Push (VM.Symbol (Builtin Div)), Call, Push (VM.Symbol (Builtin Div)), Call, Ret] >>= assertEqual "divide zero" (Right (VM.Integer 0))
        ]

testBuiltins :: Test
testBuiltins =
    TestList
        [ TestCase $ execVM [Push (VM.Float 1.44), Push (VM.Float 2.56), Push (VM.Symbol (Builtin Add)), Call, Ret] >>= assertEqual "Floats: add" (Right (VM.Float 4.0))
        , TestCase $ execVM [Push (VM.Float 10.8), Push (VM.Float 1.5), Push (VM.Symbol (Builtin Sub)), Call, Ret] >>= assertEqual "Floats: sub" (Right (VM.Float (-9.3)))
        , TestCase $ execVM [Push (VM.Float 2.5), Push (VM.Float 2.5), Push (VM.Symbol (Builtin Mul)), Call, Ret] >>= assertEqual "Floats: mul" (Right (VM.Float 6.25))
        , TestCase $ execVM [Push (VM.Float 1.4), Push (VM.Float 5.6), Push (VM.Symbol (Builtin Div)), Call, Ret] >>= assertEqual "Floats: div" (Right (VM.Float 4.0))
        , TestCase $ execVM [Push (VM.Float 1.5), Push (VM.Float 5.5), Push (VM.Symbol (Builtin Mod)), Call, Ret] >>= assertEqual "Floats: mod" (Right (VM.Float 1.0))
        , TestCase $ execVM [Push (VM.Float 1.5), Push (VM.Float 5.5), Push (VM.Symbol (Builtin Less)), Call, Ret] >>= assertEqual "Floats: less" (Right (VM.Bool False))
        , TestCase $ execVM [Push (VM.Float 1.5), Push (VM.Float 5.5), Push (VM.Symbol (Builtin LessOrEqual)), Call, Ret] >>= assertEqual "Floats: less or equal" (Right (VM.Bool False))
        , TestCase $ execVM [Push (VM.Float 1.5), Push (VM.Float 5.5), Push (VM.Symbol (Builtin Greater)), Call, Ret] >>= assertEqual "Floats: greater" (Right (VM.Bool True))
        , TestCase $ execVM [Push (VM.Float 1.5), Push (VM.Float 1.5), Push (VM.Symbol (Builtin GreaterOrEqual)), Call, Ret] >>= assertEqual "Floats: greater or equal" (Right (VM.Bool True))
        , TestCase $ execVM [Push (VM.Float 1.5), Push (VM.Float 5.5), Push (VM.Symbol (Builtin Eq)), Call, Ret] >>= assertEqual "Floats: eq" (Right (VM.Bool False))
        , TestCase $ execVM [Push (VM.Float 1.5), Push (VM.Float 5.5), Push (VM.Symbol (Builtin Neq)), Call, Ret] >>= assertEqual "Floats: neq" (Right (VM.Bool True))
        , TestCase $ execVM [Push (VM.Integer 1), Push (VM.Integer (-1)), Push (VM.Symbol (Builtin Less)), Call, Ret] >>= assertEqual "Integers: less" (Right (VM.Bool True))
        , TestCase $ execVM [Push (VM.Integer 0), Push (VM.Integer 1), Push (VM.Symbol (Builtin LessOrEqual)), Call, Ret] >>= assertEqual "Integers: less or equal" (Right (VM.Bool False))
        , TestCase $ execVM [Push (VM.Integer 5), Push (VM.Integer 15), Push (VM.Symbol (Builtin Greater)), Call, Ret] >>= assertEqual "Integers: greater" (Right (VM.Bool True))
        , TestCase $ execVM [Push (VM.Integer 18), Push (VM.String "the last integer is... "), Push (VM.Symbol (Builtin Add)), Call, Ret] >>= assertEqual "Strings: concat to an integer" (Right (VM.String "the last integer is... 18"))
        , TestCase $ execVM [Push (VM.Bool False), Push (VM.Bool True), Push (VM.Symbol (Builtin Eq)), Call, Ret] >>= assertEqual "Bools: eq" (Right (VM.Bool False))
        , TestCase $ execVM [Push (VM.Bool False), Push (VM.Bool True), Push (VM.Symbol (Builtin Neq)), Call, Ret] >>= assertEqual "Bools: neq" (Right (VM.Bool True))
        , TestCase $ execVM [Push (VM.String "hello"), Push (VM.String "hello"), Push (VM.Symbol (Builtin Eq)), Call, Ret] >>= assertEqual "Strings: eq" (Right (VM.Bool True))
        , TestCase $ execVM [Push (VM.String "hello"), Push (VM.String "hello"), Push (VM.Symbol (Builtin Neq)), Call, Ret] >>= assertEqual "Strings: neq" (Right (VM.Bool False))
        , TestCase $ execVM [Push (VM.Float 0), Push (VM.Float 1), Push (VM.Symbol (Builtin Div)), Call, Ret] >>= assertEqual "Floats: div by zero" (Left "Cannot divide by 0")
        , TestCase $ execVM [Push (VM.Float 0), Push (VM.Float 1), Push (VM.Symbol (Builtin Mod)), Call, Ret] >>= assertEqual "Floats: mod by zero" (Left "Cannot divide by 0")
        , TestCase $ execVM [Push (VM.Bool True), Push (VM.Bool False), Push (VM.Symbol (Builtin Less)), Call, Ret] >>= assertEqual "Bools: incompatible operator" (Left "Wrong data types in stack: Less with 2 booleans")
        , TestCase $ execVM [Push (VM.String "hey"), Push (VM.String "yo"), Push (VM.Symbol (Builtin GreaterOrEqual)), Call, Ret] >>= assertEqual "Strings: incompatible operator" (Left "Wrong data types in stack: GreaterOrEqual with a string as left operator")
        , TestCase $ execVM [Push (VM.Integer 2), Push (VM.String "yo"), Push (VM.Symbol (Builtin Sub)), Call, Ret] >>= assertEqual "String and Integer: incompatible operator" (Left "Wrong data types in stack: Sub with a string and an integer")
        ]

testEnvDefinesAndRedefines :: Test
testEnvDefinesAndRedefines =
    TestList
        [ TestCase $ execVM [Push (VM.Integer (-26)), DefineEnv "opp" False (Just (Function [PushArg 0, Push (VM.Integer (-1)), Push (VM.Symbol (Builtin Mul)), Call, Ret])), PushEnv "opp", Call, Ret] >>= assertEqual "basic function" (Right (VM.Integer 26))
        , TestCase $ execVM [DefineEnv "inc" False (Just (Function [PushArg 0, DefineEnv "val" False (Just (Variable (VM.Integer 2))), PushEnv "val", Push (VM.Symbol (Builtin Add)), Call, Ret])), Push (VM.Integer 2), PushEnv "inc", Call, Ret] >>= assertEqual "define inside define" (Right (VM.Integer 4))
        , TestCase $ execVM [DefineEnv "inc" False (Just (Function [PushArg 0, DefineEnv "val" False (Just (Variable (VM.Integer 2))), PushEnv "val", Push (VM.Symbol (Builtin Add)), Call, Ret])), PushEnv "val", Call, Ret] >>= assertEqual "private scopes" (Left "Environment val does not exist")
        , TestCase $ execVM [DefineEnv "idx" False (Just (Variable (VM.Integer 3))), DefineEnv "mul" False (Just (Function [PushEnv "idx", PushArg 0, Push (VM.Symbol (Builtin Mul)), Call, Ret])), Push (VM.String "hey"), PushEnv "mul", Call, Ret] >>= assertEqual "parent scope" (Right (VM.String "heyheyhey"))
        , TestCase $ execVM [DefineEnv "test" False (Just (Function [Push (VM.Bool True), Push (VM.Symbol (Builtin Eq)), Call, Ret])), PushEnv "test", Call, Ret] >>= assertEqual "error inside function" (Left "Wrong stack variables for builtin Eq")
        , TestCase $ execVM [DefineEnv "begin" False (Just (Variable (VM.String "bonjour"))), DefineEnv "begin" True (Just (Variable (VM.String "hello"))), Push (VM.String " world"), PushEnv "begin", Push (VM.Symbol (Builtin Add)), Call, Ret] >>= assertEqual "environment reassignment" (Right (VM.String "hello world"))
        , TestCase $ execVM [Push (VM.String "Hello World"), DefineEnv "message" False Nothing, Ret] >>= assertEqual "basic DefineEnv from stack" (Right VM.Void)
        , TestCase $ execVM [DefineEnv "message" False Nothing, Ret] >>= assertEqual "empty stack for DefineEnv from stack" (Left "Stack is empty for a DefineEnv from stack instruction")
        , TestCase $ execVM [Push (VM.Integer 1), Push (VM.Integer 2), Push (VM.Bool True), DefineEnv "fst" False Nothing, DefineEnv "sec" False Nothing, DefineEnv "neq" False Nothing, PushEnv "sec", Ret] >>= assertEqual "wipe stack" (Right (VM.Integer 2))
        , TestCase $ execVM [DefineEnv "test" False (Just (Variable (VM.Bool True))), EraseEnv "test", PushEnv "test", Ret] >>= assertEqual "use of erased env" (Left "Environment test does not exist")
        , TestCase $ execVM [DefineEnv "test" False (Just (Variable (VM.Bool True))), EraseEnv "test", DefineEnv "test" False (Just (Variable (VM.Bool False))), PushEnv "test", Ret] >>= assertEqual "define after erased env" (Right (VM.Bool False))
        , TestCase $ execVM [DefineEnv "test" False (Just (Variable (VM.Bool True))), EraseEnv "test", DefineEnv "test" True (Just (Variable (VM.Bool False))), PushEnv "test", Ret] >>= assertEqual "redefine after erased env" (Left "Environment test does not exist")
        , TestCase $ execVM [DefineEnv "a" False (Just (Variable (VM.Integer 0))), DefineEnv "a" False (Just (Variable (VM.String "hello"))), PushEnv "a", Ret] >>= assertEqual "double define" (Left "Environment a already exists")
        , TestCase $ execVM [DefineEnv "a" False (Just (Variable (VM.Integer 0))), Push (VM.Bool False), DefineEnv "a" False Nothing, PushEnv "a", Ret] >>= assertEqual "double define from stack" (Left "Environment a already exists")
        , TestCase $ execVM [Push (VM.Bool False), DefineEnv "a" True Nothing, PushEnv "a", Ret] >>= assertEqual "redefine from stack witout define" (Left "Environment a does not exist")
        , TestCase $ execVM [EraseEnv "test", DefineEnv "test" True Nothing, PushEnv "test", Ret] >>= assertEqual "erase before define" (Left "Environment test does not exist")
        ]

testJumps :: Test
testJumps =
    TestList
        [ TestCase $ execVM [Jump 1 Nothing, Ret, Push (VM.Integer 1), Ret] >>= assertEqual "basic jump" (Right (VM.Integer 1))
        , TestCase $ execVM [Push (VM.Bool True), Jump 2 (Just False), Push (VM.Integer 1), Ret, Push (VM.Integer 2), Ret] >>= assertEqual "basic jump if false" (Right (VM.Integer 1))
        , TestCase $ execVM [Jump 1 (Just False), Push (VM.Integer 1), Ret, Push (VM.Integer 2), Ret] >>= assertEqual "jump with empty stack" (Left "Stack is empty for a conditional jump")
        , TestCase $ execVM [Push (VM.Integer 2), Jump 3 (Just False), Push (VM.Integer 1), Ret, Push (VM.Integer 2), Ret] >>= assertEqual "jump with an integer" (Right (VM.Integer 1))
        , TestCase $ execVM [Push (VM.Integer 0), Jump 2 (Just False), Push (VM.Integer 1), Ret, Push (VM.Integer 2), Ret] >>= assertEqual "jump with an integer" (Right (VM.Integer 2))
        , TestCase $ execVM [Push (VM.String ""), Jump 5 Nothing, Push (VM.Integer 2), Push (VM.Integer 0), Push (VM.Symbol (Builtin Div)), Call, Ret, Jump (-6) (Just False)] >>= assertEqual "jump with a negative integer" (Right (VM.Integer 0))
        , TestCase $ execVM [Push (VM.String ""), Jump 5 Nothing, Push (VM.Integer 2), Push (VM.Integer 0), Push (VM.Symbol (Builtin Div)), Call, Ret, Jump (-9) Nothing] >>= assertEqual "too long negative jump" (Left "Invalid number of instructions")
        , TestCase $ execVM [Push (VM.Bool False), Jump (-1) (Just False), Push (VM.Integer 1), Ret, Push (VM.Integer 2), Ret] >>= assertEqual "invalid jump" (Left "Invalid number of instructions")
        , TestCase $ execVM [Push (VM.Bool True), Jump 5 (Just False), Push (VM.Integer 1), Ret, Push (VM.Integer 2), Ret] >>= assertEqual "too long jump" (Left "Invalid number of instructions")
        , TestCase $ execVM [Push VM.Void, Jump 1 (Just True), Ret, Push (VM.Integer 1), Ret] >>= assertEqual "VM.Void as a value" (Right VM.Void)
        ]

testFunctions :: Test
testFunctions =
    TestList
        [ TestCase $ execVM [DefineEnv "fact" False (Just (Function [PushArg 0, Push (VM.Integer 1), Push (VM.Symbol (Builtin Eq)), Call, Jump 2 (Just False), Push (VM.Integer 1), Ret, Push (VM.Integer 1), PushArg 0, Push (VM.Symbol (Builtin Sub)), Call, PushEnv "fact", Call, PushArg 0, Push (VM.Symbol (Builtin Mul)), Call, Ret])), Push (VM.Integer 5), PushEnv "fact", Call, Ret] >>= assertEqual "factorial" (Right (VM.Integer 120))
        , TestCase $ execVM [DefineEnv "a" False (Just (Variable (VM.Integer 0))), DefineEnv "b" False (Just (Variable (VM.String "hello"))), Push (VM.Bool False), Jump 10 (Just False), Push (VM.Integer 1), PushEnv "a", Push (VM.Symbol (Builtin Add)), Call, DefineEnv "a" True Nothing, Push (VM.Integer 2), PushEnv "b", Push (VM.Symbol (Builtin Mul)), Call, DefineEnv "b" True Nothing, Push (VM.Integer 2), PushEnv "a", Push (VM.Symbol (Builtin GreaterOrEqual)), Call, Jump (-15) (Just False), PushEnv "b", Ret] >>= assertEqual "for loop" (Right (VM.String "hellohellohellohello"))
        , TestCase $ execVM [Push (VM.Integer 10), PushEnv "print", Call, Ret] >>= assertEqual "basic print" (Right VM.Void)
        , TestCase $ execVM [PushEnv "print", Call, Ret] >>= assertEqual "print with empty stack" (Left "Stack is empty for print instruction")
        ]
