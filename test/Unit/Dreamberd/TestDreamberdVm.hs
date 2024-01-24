module Unit.Dreamberd.TestDreamberdVm (testDreamberdVm) where

import Dreamberd.Vm (
    Builtin (..),
    Call (..),
    DefineEnvType (..),
    Env (..),
    Insts (..),
    Operator (..),
    Value (..),
    Variable (..),
    exec,
    execVM,
 )
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
        , TestCase $ execVM [Push (Integer 1)] >>= assertEqual "basic push" (Right Void)
        , TestCase $ execVM [Push (Integer 1), Ret] >>= assertEqual "basic push with ret" (Right (Integer 1))
        , TestCase $ execVM [Push (Integer 1), Push (Integer 2), Push (Symbol (Operator Add)), Call, Ret] >>= assertEqual "basic operation" (Right (Integer 3))
        , TestCase $ execVM [Push (Integer 1), Push (Integer 2), Push (Symbol (Operator Neq)), Call, Jump 2 (Just False), Push (Bool False), Ret, Push (Bool True), Ret] >>= assertEqual "basic if true" (Right (Bool False))
        , TestCase $ execVM [Push (Integer 1), Push (Integer 2), Push (Symbol (Operator Eq)), Call, Jump 2 (Just False), Push (Bool False), Ret, Push (Bool True), Ret] >>= assertEqual "basic if false" (Right (Bool True))
        , TestCase $ exec [] [] [] [Push (Integer 0), Ret] (-1) 0 >>= assertEqual "wrong index: negative" (Left "Instructions index out of bounds")
        , TestCase $ exec [] [] [] [Push (Integer 0), Ret] 3 0 >>= assertEqual "wrong index: too long" (Left "Instructions index out of bounds")
        ]

testStackPushes :: Test
testStackPushes =
    TestList
        [ TestCase $ execVM [PushArg 0] >>= assertEqual "PushArg without args" (Left "Argument index out of bounds")
        , TestCase $ exec [] [Variable (Integer 1) 0] [] [PushArg 0, Ret] 0 0 >>= assertEqual "push from arg" (Right (Variable (Integer 1) 0))
        , TestCase $ exec [] [Variable (Integer 1) 0] [] [PushArg (-1), Ret] 0 0 >>= assertEqual "push negative" (Left "Argument index out of bounds")
        , TestCase $ exec [Env{identifier = "ret", value = Variable (Lambda 1 [PushArg 0, Ret]) 0}] [] [] [Push (Integer 10), PushEnv "ret", Call, Ret] 0 0 >>= assertEqual "push arg from function" (Right (Variable (Integer 10) 0))
        , TestCase $ exec [Env{identifier = "ret", value = Variable (Lambda 1 [PushArg 0, Ret]) 0}] [] [] [Push (Integer 10), PushEnv "res", Call, Ret] 0 0 >>= assertEqual "push unknown env" (Left "Environment res does not exist")
        , TestCase $ exec [Env{identifier = "const", value = Variable (Bool True) 0}] [] [] [Push (Integer 10), PushEnv "const", Ret] 0 0 >>= assertEqual "push constant env" (Right (Variable (Bool True) 0))
        ]

testCalls :: Test
testCalls =
    TestList
        [ TestCase $ execVM [Push (Integer 0), Call] >>= assertEqual "not a symbol" (Left "Stack argument is not a symbol or a lambda")
        , TestCase $ execVM [Call] >>= assertEqual "empty stack" (Left "Stack is empty for a Call instruction")
        , TestCase $ execVM [Push (Integer 2), Push (Integer 2), Push (Integer 6), Push (Symbol (Operator Sub)), Call, Push (Integer 20), Push (Symbol (Operator Div)), Call, Push (Symbol (Operator Mod)), Call, Ret] >>= assertEqual "mixed operations" (Right (Integer 1))
        , TestCase $ execVM [Push (Integer 2), Push (Integer 0), Push (Symbol (Operator Mul)), Call, Push (Integer 1), Push (Symbol (Operator Div)), Call] >>= assertEqual "divide by zero" (Left "Cannot divide by 0")
        , TestCase $ execVM [Push (Integer 0), Push (Integer 0), Push (Symbol (Operator Mod)), Call, Ret] >>= assertEqual "mod by zero" (Left "Cannot divide by 0")
        , TestCase $ execVM [Push (Integer 1), Push (Integer 2), Push (Integer 0), Push (Symbol (Operator Div)), Call, Push (Symbol (Operator Div)), Call, Ret] >>= assertEqual "divide zero" (Right (Integer 0))
        ]

testBuiltins :: Test
testBuiltins =
    TestList
        [ TestCase $ execVM [Push (String "bonsoir"), Push (Bool True), Push (Symbol (Operator Add)), Call, Ret] >>= assertEqual "Strings: concat" (Right (String "Truebonsoir"))
        , TestCase $ execVM [Push (Integer 3), Push (Integer 2), Push (Symbol (Operator Pow)), Call, Ret] >>= assertEqual "Integers: pow" (Right (Integer 8))
        , TestCase $ execVM [Push (Float 1.44), Push (Float 2.56), Push (Symbol (Operator Add)), Call, Ret] >>= assertEqual "Floats: add" (Right (Float 4.0))
        , TestCase $ execVM [Push (Float 10.8), Push (Float 1.5), Push (Symbol (Operator Sub)), Call, Ret] >>= assertEqual "Floats: sub" (Right (Float (-9.3)))
        , TestCase $ execVM [Push (Float 2.5), Push (Float 2.5), Push (Symbol (Operator Mul)), Call, Ret] >>= assertEqual "Floats: mul" (Right (Float 6.25))
        , TestCase $ execVM [Push (Float 1.4), Push (Float 5.6), Push (Symbol (Operator Div)), Call, Ret] >>= assertEqual "Floats: div" (Right (Float 4.0))
        , TestCase $ execVM [Push (Float 1.5), Push (Float 5.5), Push (Symbol (Operator Mod)), Call, Ret] >>= assertEqual "Floats: mod" (Right (Float 1.0))
        , TestCase $ execVM [Push (Float 10.5), Push (Float 2.2), Push (Symbol (Operator Pow)), Call, Ret] >>= assertEqual "Floats: pow" (Right (Float 3939.473184462729))
        , TestCase $ execVM [Push (Float 1.5), Push (Float 5.5), Push (Symbol (Operator Less)), Call, Ret] >>= assertEqual "Floats: less" (Right (Bool False))
        , TestCase $ execVM [Push (Float 1.5), Push (Float 5.5), Push (Symbol (Operator LessOrEqual)), Call, Ret] >>= assertEqual "Floats: less or equal" (Right (Bool False))
        , TestCase $ execVM [Push (Float 1.5), Push (Float 5.5), Push (Symbol (Operator Greater)), Call, Ret] >>= assertEqual "Floats: greater" (Right (Bool True))
        , TestCase $ execVM [Push (Float 1.5), Push (Float 1.5), Push (Symbol (Operator GreaterOrEqual)), Call, Ret] >>= assertEqual "Floats: greater or equal" (Right (Bool True))
        , TestCase $ execVM [Push (Float 1.5), Push (Float 5.5), Push (Symbol (Operator Eq)), Call, Ret] >>= assertEqual "Floats: eq" (Right (Bool False))
        , TestCase $ execVM [Push (Float 1.5), Push (Float 5.5), Push (Symbol (Operator Neq)), Call, Ret] >>= assertEqual "Floats: neq" (Right (Bool True))
        , TestCase $ execVM [Push (Integer 1), Push (Integer (-1)), Push (Symbol (Operator Less)), Call, Ret] >>= assertEqual "Integers: less" (Right (Bool True))
        , TestCase $ execVM [Push (Integer 0), Push (Integer 1), Push (Symbol (Operator LessOrEqual)), Call, Ret] >>= assertEqual "Integers: less or equal" (Right (Bool False))
        , TestCase $ execVM [Push (Integer 5), Push (Integer 15), Push (Symbol (Operator Greater)), Call, Ret] >>= assertEqual "Integers: greater" (Right (Bool True))
        , TestCase $ execVM [Push (Integer 18), Push (String "the last integer is... "), Push (Symbol (Operator Add)), Call, Ret] >>= assertEqual "Strings: concat to an integer" (Right (String "the last integer is... 18"))
        , TestCase $ execVM [Push (Bool False), Push (Bool True), Push (Symbol (Operator Eq)), Call, Ret] >>= assertEqual "Bools: eq" (Right (Bool False))
        , TestCase $ execVM [Push (Bool False), Push (Bool True), Push (Symbol (Operator Neq)), Call, Ret] >>= assertEqual "Bools: neq" (Right (Bool True))
        , TestCase $ execVM [Push (Bool False), Push (Bool True), Push (Symbol (Operator And)), Call, Ret] >>= assertEqual "Bools: and" (Right (Bool False))
        , TestCase $ execVM [Push (Bool False), Push (Bool False), Push (Symbol (Operator Or)), Call, Ret] >>= assertEqual "Bools: or" (Right (Bool False))
        , TestCase $ execVM [Push (Bool True), Push (Bool True), Push (Symbol (Operator Xor)), Call, Ret] >>= assertEqual "Bools: xor" (Right (Bool False))
        , TestCase $ execVM [Push (String "hello"), Push (String "hello"), Push (Symbol (Operator Eq)), Call, Ret] >>= assertEqual "Strings: eq" (Right (Bool True))
        , TestCase $ execVM [Push (String "hello"), Push (String "hello"), Push (Symbol (Operator Neq)), Call, Ret] >>= assertEqual "Strings: neq" (Right (Bool False))
        , TestCase $ execVM [Push (Float 0), Push (Float 1), Push (Symbol (Operator Div)), Call, Ret] >>= assertEqual "Floats: div by zero" (Left "Cannot divide by 0")
        , TestCase $ execVM [Push (Float 0), Push (Float 1), Push (Symbol (Operator Mod)), Call, Ret] >>= assertEqual "Floats: mod by zero" (Left "Cannot divide by 0")
        , TestCase $ execVM [Push (Bool True), Push (Bool False), Push (Symbol (Operator Less)), Call, Ret] >>= assertEqual "Bools: incompatible operator" (Left "Wrong data types in stack: Less with 2 booleans")
        , TestCase $ execVM [Push (String "hey"), Push (String "yo"), Push (Symbol (Operator GreaterOrEqual)), Call, Ret] >>= assertEqual "Strings: incompatible operator" (Left "Wrong data types in stack: GreaterOrEqual with a string as left operator")
        , TestCase $ execVM [Push (Integer 2), Push (String "yo"), Push (Symbol (Operator Sub)), Call, Ret] >>= assertEqual "String and Integer: incompatible operator" (Left "Wrong data types in stack: Sub with a string and an integer")
        , TestCase $ execVM [Push (Integer 1), Push (Integer 56), Push (Symbol (Operator Xor)), Call, Ret] >>= assertEqual "Integers: incompatible operator" (Left "Wrong data types in stack: Xor with 2 integers")
        , TestCase $ execVM [Push (Float 1.2), Push (Float 7.0), Push (Symbol (Operator Or)), Call, Ret] >>= assertEqual "Integers: incompatible operator" (Left "Wrong data types in stack: Or with 2 floats")
        ]

testEnvDefinesAndRedefines :: Test
testEnvDefinesAndRedefines =
    TestList
        [ TestCase $ execVM [Push (Integer (-26)), DefineEnv "opp" Define (Just (Lambda 1 [PushArg 0, Push (Integer (-1)), Push (Symbol (Operator Mul)), Call, Ret])), PushEnv "opp", Call, Ret] >>= assertEqual "basic function" (Right (Integer 26))
        , TestCase $ execVM [DefineEnv "inc" Define (Just (Lambda 1 [PushArg 0, DefineEnv "val" Define (Just (Integer 2)), PushEnv "val", Push (Symbol (Operator Add)), Call, Ret])), Push (Integer 2), PushEnv "inc", Call, Ret] >>= assertEqual "define inside define" (Right (Integer 4))
        , TestCase $ execVM [DefineEnv "inc" Define (Just (Lambda 1 [PushArg 0, DefineEnv "val" Define (Just (Integer 2)), PushEnv "val", Push (Symbol (Operator Add)), Call, Ret])), PushEnv "val", Call, Ret] >>= assertEqual "private scopes" (Left "Environment val does not exist")
        , TestCase $ execVM [DefineEnv "idx" Define (Just (Integer 3)), DefineEnv "mul" Define (Just (Lambda 1 [PushEnv "idx", PushArg 0, Push (Symbol (Operator Mul)), Call, Ret])), Push (String "hey"), PushEnv "mul", Call, Ret] >>= assertEqual "parent scope" (Right (String "heyheyhey"))
        , TestCase $ execVM [DefineEnv "test" Override (Just (Lambda 0 [Push (Bool True), Push (Symbol (Operator Eq)), Call, Ret])), PushEnv "test", Call, Ret] >>= assertEqual "error inside function" (Left "Stack argument is not a symbol or a lambda")
        , TestCase $ execVM [DefineEnv "begin" Define (Just (String "bonjour")), DefineEnv "begin" Redefine (Just (String "hello")), Push (String " world"), PushEnv "begin", Push (Symbol (Operator Add)), Call, Ret] >>= assertEqual "environment reassignment" (Right (String "hello world"))
        , TestCase $ execVM [Push (String "Hello World"), DefineEnv "message" Define Nothing, Ret] >>= assertEqual "basic DefineEnv from stack" (Right Void)
        , TestCase $ execVM [DefineEnv "message" Define Nothing, Ret] >>= assertEqual "empty stack for DefineEnv from stack" (Left "Stack is empty for a DefineEnv from stack instruction")
        , TestCase $ execVM [Push (Integer 1), Push (Integer 2), Push (Bool True), DefineEnv "fst" Override Nothing, DefineEnv "sec" Define Nothing, DefineEnv "neq" Define Nothing, PushEnv "sec", Ret] >>= assertEqual "wipe stack" (Right (Integer 2))
        , TestCase $ execVM [DefineEnv "test" Define (Just (Bool True)), EraseEnv "test", PushEnv "test", Ret] >>= assertEqual "use of erased env" (Left "Environment test does not exist")
        , TestCase $ execVM [DefineEnv "test" Define (Just (Bool True)), EraseEnv "test", DefineEnv "test" Define (Just (Bool False)), PushEnv "test", Ret] >>= assertEqual "define after erased env" (Right (Bool False))
        , TestCase $ execVM [DefineEnv "test" Define (Just (Bool True)), EraseEnv "test", DefineEnv "test" Redefine (Just (Bool False)), PushEnv "test", Ret] >>= assertEqual "redefine after erased env" (Left "Environment test does not exist")
        , TestCase $ execVM [DefineEnv "a" Define (Just (Integer 0)), DefineEnv "a" Define (Just (String "hello")), PushEnv "a", Ret] >>= assertEqual "double define" (Left "Environment a already exists")
        , TestCase $ execVM [DefineEnv "a" Define (Just (Integer 0)), Push (Bool False), DefineEnv "a" Define Nothing, PushEnv "a", Ret] >>= assertEqual "double define from stack" (Left "Environment a already exists")
        , TestCase $ execVM [Push (Bool False), DefineEnv "a" Redefine Nothing, PushEnv "a", Ret] >>= assertEqual "redefine from stack witout define" (Left "Environment a does not exist")
        , TestCase $ execVM [EraseEnv "test", DefineEnv "test" Redefine Nothing, PushEnv "test", Ret] >>= assertEqual "erase before define" (Left "Environment test does not exist")
        ]

testJumps :: Test
testJumps =
    TestList
        [ TestCase $ execVM [Jump 1 Nothing, Ret, Push (Integer 1), Ret] >>= assertEqual "basic jump" (Right (Integer 1))
        , TestCase $ execVM [Push (Bool True), Jump 2 (Just False), Push (Integer 1), Ret, Push (Integer 2), Ret] >>= assertEqual "basic jump if false" (Right (Integer 1))
        , TestCase $ execVM [Jump 1 (Just False), Push (Integer 1), Ret, Push (Integer 2), Ret] >>= assertEqual "jump with empty stack" (Left "Stack is empty for a conditional jump")
        , TestCase $ execVM [Push (Integer 2), Jump 3 (Just False), Push (Integer 1), Ret, Push (Integer 2), Ret] >>= assertEqual "jump with an integer" (Right (Integer 1))
        , TestCase $ execVM [Push (Integer 0), Jump 2 (Just False), Push (Integer 1), Ret, Push (Integer 2), Ret] >>= assertEqual "jump with an integer" (Right (Integer 2))
        , TestCase $ execVM [Push (String ""), Jump 5 Nothing, Push (Integer 2), Push (Integer 0), Push (Symbol (Operator Div)), Call, Ret, Jump (-6) (Just False)] >>= assertEqual "jump with a negative integer" (Right (Integer 0))
        , TestCase $ execVM [Push (String ""), Jump 5 Nothing, Push (Integer 2), Push (Integer 0), Push (Symbol (Operator Div)), Call, Ret, Jump (-9) Nothing] >>= assertEqual "too long negative jump" (Left "Invalid number of instructions")
        , TestCase $ execVM [Push (Bool False), Jump (-1) (Just False), Push (Integer 1), Ret, Push (Integer 2), Ret] >>= assertEqual "invalid jump" (Left "Invalid number of instructions")
        , TestCase $ execVM [Push (Bool True), Jump 5 (Just False), Push (Integer 1), Ret, Push (Integer 2), Ret] >>= assertEqual "too long jump" (Left "Invalid number of instructions")
        , TestCase $ execVM [Push Void, Jump 1 (Just True), Ret, Push (Integer 1), Ret] >>= assertEqual "Void as a value" (Right Void)
        , TestCase $ execVM [Push (Bool True), Jump 3 (Just True), Ret, Push (Integer 1), Ret] >>= assertEqual "Jump to program end" (Right Void)
        ]

testFunctions :: Test
testFunctions =
    TestList
        [ TestCase $ execVM [DefineEnv "fact" Define (Just (Lambda 1 [PushArg 0, Push (Integer 1), Push (Symbol (Operator Eq)), Call, Jump 2 (Just False), Push (Integer 1), Ret, Push (Integer 1), PushArg 0, Push (Symbol (Operator Sub)), Call, PushEnv "fact", Call, PushArg 0, Push (Symbol (Operator Mul)), Call, Ret])), Push (Integer 5), PushEnv "fact", Call, Ret] >>= assertEqual "factorial" (Right (Integer 120))
        , TestCase $ execVM [DefineEnv "a" Define (Just (Integer 0)), DefineEnv "b" Define (Just (String "hello")), Push (Bool False), Jump 10 (Just False), Push (Integer 1), PushEnv "a", Push (Symbol (Operator Add)), Call, DefineEnv "a" Redefine Nothing, Push (Integer 2), PushEnv "b", Push (Symbol (Operator Mul)), Call, DefineEnv "b" Redefine Nothing, Push (Integer 2), PushEnv "a", Push (Symbol (Operator GreaterOrEqual)), Call, Jump (-15) (Just False), PushEnv "b", Ret] >>= assertEqual "for loop" (Right (String "hellohellohellohello"))
        , TestCase $ execVM [Push (Integer 10), Push (Symbol (Builtin Print)), Call, Ret] >>= assertEqual "basic print" (Right Void)
        , TestCase $ execVM [Push (Symbol (Builtin Print)), Call, Ret] >>= assertEqual "print with empty stack" (Left "Stack is empty for a print instruction")
        , TestCase $ execVM [Push (Bool True), Push (Symbol (Builtin Error)), Call, Ret] >>= assertEqual "basic error" (Right Void)
        , TestCase $ execVM [Push (Symbol (Builtin Error)), Call, Ret] >>= assertEqual "error with empty stack" (Left "Stack is empty for an error instruction")
        ]
