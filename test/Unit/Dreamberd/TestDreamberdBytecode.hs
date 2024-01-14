module Unit.Dreamberd.TestDreamberdBytecode (testDreamberdBytecode) where

import Dreamberd.Bytecode.Encode (transpileCall, transpileInstruction, transpileInt, transpileIntoBytecode, transpileString)
import Dreamberd.Vm (
    Call (..),
    EnvValue (..),
    Insts (..),
    Operator (..),
    Value (..),
 )
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
        , TestCase $ assertEqual "Transpile Float" (transpileInstruction (Push (Float 1.0))) "\SOH\DC2Q\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\DLE\NUL\NUL\NUL\NUL\NUL\NUL\255\255\255\255\255\255\255\204"
        , TestCase $ assertEqual "Transpile String" (transpileString "Hello") "\x00\x00\x00\x00\x00\x00\x00\x05Hello"
        , TestCase $ assertEqual "Transpile String 2" (transpileString "World") "\x00\x00\x00\x00\x00\x00\x00\x05World"
        , TestCase $ assertEqual "Transpile Empty String" (transpileString "") "\x00\x00\x00\x00\x00\x00\x00\x00"
        , TestCase $ assertEqual "Transpile Call Add" (transpileCall (Builtin Add)) "\x21\x31"
        , TestCase $ assertEqual "Transpile Call FunctionName" (transpileCall (FunctionName "test")) "\x22\x00\x00\x00\x00\x00\x00\x00\x04test"
        , TestCase $ assertEqual "Transpile Instruction Push" (transpileInstruction (Push (Integer 1))) "\x01\x11\x00\x00\x00\x00\x00\x00\x00\x01"
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
        [ TestCase $ assertEqual "basic bytecode" (transpileIntoBytecode [Push (Integer 1), Ret]) "db4\n\x01\x11\x00\x00\x00\x00\x00\x00\x00\x01\x08\n"
        , TestCase $ assertEqual "test equality between floats" (transpileIntoBytecode [Push (Float 2.0), Push (Float 3.4), Push (Symbol (Builtin Eq)), Call, Ret]) "db4\n\SOH\DC2Q\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\DLE\NUL\NUL\NUL\NUL\NUL\NUL\255\255\255\255\255\255\255\205\SOH\DC2Q\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\ESC333333\255\255\255\255\255\255\255\205\SOH\NAK!7\EOT\b\n"
        , TestCase $ assertEqual "abs function" (transpileIntoBytecode [DefineEnv "abs" False (Just (Function [Push (Integer 0), PushArg 0, Push (Symbol (Builtin Less)), Call, Jump 5 (Just False), Push (Integer (-1)), PushArg 0, Push (Symbol (Builtin Mul)), Call, Ret, PushArg 0, Ret]))]) "db4\n\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETXabsRA\NUL\NUL\NUL\NUL\NUL\NUL\NULE\SOH\DC1\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NAK!9\EOT\a\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQR\SOH\DC1\255\255\255\255\255\255\255\255\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NAK!3\EOT\b\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\n"
        , TestCase $ assertEqual "function call" (transpileIntoBytecode [DefineEnv "abs" False (Just (Function [Push (Integer 0), PushArg 0, Push (Symbol (Builtin Less)), Call, Jump 5 (Just False), Push (Integer (-1)), PushArg 0, Push (Symbol (Builtin Mul)), Call, Ret, PushArg 0, Ret])), Push (Integer (-2)), PushEnv "abs", Call, Ret]) "db4\n\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETXabsRA\NUL\NUL\NUL\NUL\NUL\NUL\NULE\SOH\DC1\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NAK!9\EOT\a\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQR\SOH\DC1\255\255\255\255\255\255\255\255\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\NAK!3\EOT\b\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\b\SOH\DC1\255\255\255\255\255\255\255\254\ETX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ETXabs\EOT\b\n"
        , TestCase $ assertEqual "defines" (transpileIntoBytecode [Push (Float (-5.2)), DefineEnv "a" False Nothing, DefineEnv "a" True (Just (Function [PushArg 0, Push (Bool False), Push (Symbol (Builtin Neq)), Call, Ret]))]) "db4\n\SOH\DC2R\NUL\NUL\NUL\NUL\NUL\NUL\NUL\a\DC4\204\204\204\204\204\205\255\255\255\255\255\255\255\206\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOHaRS\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOHaQA\NUL\NUL\NUL\NUL\NUL\NUL\NUL\DC2\STX\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\DC3\NUL\SOH\NAK!8\EOT\b\n"
        , TestCase $ assertEqual "jump too long (should not raise)" (transpileIntoBytecode [Push (Bool True), Jump 46 (Just True), Ret]) "db4\n\SOH\DC3\SOH\a\NUL\NUL\NUL\NUL\NUL\NUL\NUL.Q\b\n"
        , TestCase $ assertEqual "invalid variables for op (should not raise)" (transpileIntoBytecode [Push Void, Push (String "bonsoir a tous"), Push (Bool True), Push (Symbol (Builtin Eq)), Call, Ret]) "db4\n\SOH\SYN\SOH\DC4\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SObonsoir a tous\SOH\DC3\SOH\SOH\NAK!7\EOT\b\n"
        , TestCase $ assertEqual "invalid redefine (should not raise)" (transpileIntoBytecode [DefineEnv "b" False Nothing, DefineEnv "a" True (Just (Variable (String "haha"))), Ret]) "db4\n\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOHbRS\ENQ\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOHaQB\DC4\NUL\NUL\NUL\NUL\NUL\NUL\NUL\EOThaha\b\n"
        ]
