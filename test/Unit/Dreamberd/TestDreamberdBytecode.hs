module Unit.Dreamberd.TestDreamberdBytecode (testDreamberdBytecode) where

import Dreamberd.Bytecode.Encode (transpileCall, transpileInstruction, transpileInt, transpileIntoBytecode, transpileString)
import Dreamberd.Vm
import Test.HUnit (Test (..), assertEqual)

testDreamberdBytecode :: Test
testDreamberdBytecode =
    TestList
        [ testTypesTranspilation
        , testTranspilation
        ]

testTypesTranspilation :: Test
testTypesTranspilation =
    TestList
        [ TestCase $ assertEqual "Transpile Int" (transpileInt 1) "\x00\x00\x00\x00\x00\x00\x00\x01"
        , TestCase $ assertEqual "Transpile Int Zero" (transpileInt 0) "\x00\x00\x00\x00\x00\x00\x00\x00"
        , TestCase $ assertEqual "Transpile Int Negative" (transpileInt (-1)) "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
        , TestCase $ assertEqual "Transpile String" (transpileString "Hello") "\x00\x00\x00\x00\x00\x00\x00\x05Hello"
        , TestCase $ assertEqual "Transpile String 2" (transpileString "World") "\x00\x00\x00\x00\x00\x00\x00\x05World"
        , TestCase $ assertEqual "Transpile Empty String" (transpileString "") "\x00\x00\x00\x00\x00\x00\x00\x00"
        , TestCase $ assertEqual "Transpile Call Add" (transpileCall (Builtin Add)) "\x21\x31"
        , TestCase $ assertEqual "Transpile Call FunctionName" (transpileCall (FunctionName "test")) "\x22\x00\x00\x00\x00\x00\x00\x00\x04test"
        , TestCase $ assertEqual "Transpile Instruction Push" (transpileInstruction (Push (Number 1))) "\x01\x11\x00\x00\x00\x00\x00\x00\x00\x01"
        , TestCase $ assertEqual "Transpile Instruction PushArg" (transpileInstruction (PushArg 1)) "\x02\x00\x00\x00\x00\x00\x00\x00\x01"
        , TestCase $ assertEqual "Transpile Instruction PushEnv" (transpileInstruction (PushEnv "test")) "\x03\x00\x00\x00\x00\x00\x00\x00\x04test"
        , TestCase $ assertEqual "Transpile Instruction Call" (transpileInstruction Call) "\x04"
        , TestCase $ assertEqual "Transpile Instruction DefineEnv with value" (transpileInstruction (DefineEnv "test" False (Just (Function [])))) "\x05\x00\x00\x00\x00\x00\x00\x00\x04test\x52\x41\x00\x00\x00\x00\x00\x00\x00\x00"
        , TestCase $ assertEqual "Transpile Instruction DefineEnv from stack" (transpileInstruction (DefineEnv "test" False Nothing)) "\x05\x00\x00\x00\x00\x00\x00\x00\x04test\x52\x53"
        , TestCase $ assertEqual "Transpile Instruction EraseEnv" (transpileInstruction (EraseEnv "yo")) "\x06\x00\x00\x00\x00\x00\x00\x00\x02yo"
        , TestCase $ assertEqual "Transpile Instruction Jump" (transpileInstruction (Jump 1 Nothing)) "\x07\x00\x00\x00\x00\x00\x00\x00\x01\x53"
        , TestCase $ assertEqual "Transpile Instruction Ret" (transpileInstruction Ret) "\x08"
        ]

testTranspilation :: Test
testTranspilation =
    TestList
        [ TestCase $ assertEqual "Transpile Into Bytecode" (transpileIntoBytecode [Push (Number 1), Ret]) "db4\n\x01\x11\x00\x00\x00\x00\x00\x00\x00\x01\x08\n"
        ]
