module Unit.Dreamberd.TestDreamberdBytecode (testDreamberdBytecode) where

import Dreamberd.Bytecode.Decode (getFromBytecode)
import Dreamberd.Bytecode.Encode (transpileCall, transpileInstruction, transpileInt, transpileIntoBytecode, transpileString)
import Dreamberd.Vm (
    Builtin (..),
    Call (..),
    DefineEnvType (..),
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
        , testEncodeDecode
        , testDecodeErrorHandling
        ]

testTypesTranspilation :: Test
testTypesTranspilation =
    TestList
        [ TestCase $ assertEqual "Transpile Int" (transpileInt 1 8) "\x00\x00\x00\x00\x00\x00\x00\x01"
        , TestCase $ assertEqual "Transpile Int Zero" (transpileInt 0 8) "\x00\x00\x00\x00\x00\x00\x00\x00"
        , TestCase $ assertEqual "Transpile Int Negative" (transpileInt (-1) 8) "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
        , TestCase $ assertEqual "Transpile Float" (transpileInstruction (Push (Float 1.0))) "\x01\DC2Q\x01\a\DLE\NUL\NUL\NUL\NUL\NUL\NUL\255\255\255\255\255\255\255\204"
        , TestCase $ assertEqual "Transpile String" (transpileString "Hello") "\x01\x05Hello"
        , TestCase $ assertEqual "Transpile Empty String" (transpileString "") "\x01\x00"
        , TestCase $ assertEqual "Transpile Call Add" (transpileCall (Operator Add)) "\x21\x31"
        , TestCase $ assertEqual "Transpile Call Print" (transpileCall (Builtin Print)) "\x22\x26"
        , TestCase $ assertEqual "Transpile Instruction Push" (transpileInstruction (Push (Integer 1))) "\x01\x11\x00\x00\x00\x00\x00\x00\x00\x01"
        , TestCase $ assertEqual "Transpile Instruction PushArg" (transpileInstruction (PushArg 1)) "\x02\x01\x01"
        , TestCase $ assertEqual "Transpile Instruction PushEnv" (transpileInstruction (PushEnv "test")) "\x03\x01\x04test"
        , TestCase $ assertEqual "Transpile Instruction Call" (transpileInstruction Call) "\x04"
        , TestCase $ assertEqual "Transpile Instruction DefineEnv with value" (transpileInstruction (DefineEnv "test" Define (Just (Lambda 0 [])))) "\ENQ\x01\x04testAQ\SYN\x01\x00\x01\x00"
        , TestCase $ assertEqual "Transpile Instruction DefineEnv from stack" (transpileInstruction (DefineEnv "test" Define Nothing)) "\x05\x01\x04test\x41\x53"
        , TestCase $ assertEqual "Transpile Instruction EraseEnv" (transpileInstruction (EraseEnv "yo")) "\x06\x01\x02yo"
        , TestCase $ assertEqual "Transpile Instruction Jump" (transpileInstruction (Jump 1 Nothing)) "\x07\x00\x00\x00\x00\x00\x00\x00\x01\x53"
        , TestCase $ assertEqual "Transpile Instruction Ret" (transpileInstruction Ret) "\x08"
        ]

testTranspilation :: Test
testTranspilation =
    TestList
        [ TestCase $ assertEqual "basic bytecode" (transpileIntoBytecode [Push (Integer 1), Ret]) "db4\n\x01\x11\x00\x00\x00\x00\x00\x00\x00\x01\x08\n"
        , TestCase $ assertEqual "test equality between floats" (transpileIntoBytecode [Push (Float 2.0), Push (Float 3.4), Push (Symbol (Operator Eq)), Call, Ret]) "db4\n\SOH\DC2Q\SOH\a\DLE\NUL\NUL\NUL\NUL\NUL\NUL\255\255\255\255\255\255\255\205\SOH\DC2Q\SOH\a\ESC333333\255\255\255\255\255\255\255\205\SOH\NAK!7\EOT\b\n"
        , TestCase $ assertEqual "abs function" (transpileIntoBytecode [DefineEnv "abs" Define (Just (Lambda 1 [Push (Integer 0), PushArg 0, Push (Symbol (Operator Less)), Call, Jump 5 (Just False), Push (Integer (-1)), PushArg 0, Push (Symbol (Operator Mul)), Call, Ret, PushArg 0, Ret]))]) "db4\n\ENQ\SOH\ETXabsAQ\SYN\SOH\SOH\SOH3\SOH\DC1\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\SOH\NUL\SOH\NAK!9\EOT\a\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQR\SOH\DC1\255\255\255\255\255\255\255\255\STX\SOH\NUL\SOH\NAK!3\EOT\b\STX\SOH\NUL\b\n"
        , TestCase $ assertEqual "function call" (transpileIntoBytecode [DefineEnv "abs" Define (Just (Lambda 1 [Push (Integer 0), PushArg 0, Push (Symbol (Operator Less)), Call, Jump 5 (Just False), Push (Integer (-1)), PushArg 0, Push (Symbol (Operator Mul)), Call, Ret, PushArg 0, Ret])), Push (Integer (-2)), PushEnv "abs", Call, Ret]) "db4\n\ENQ\SOH\ETXabsAQ\SYN\SOH\SOH\SOH3\SOH\DC1\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\STX\SOH\NUL\SOH\NAK!9\EOT\a\NUL\NUL\NUL\NUL\NUL\NUL\NUL\ENQR\SOH\DC1\255\255\255\255\255\255\255\255\STX\SOH\NUL\SOH\NAK!3\EOT\b\STX\SOH\NUL\b\SOH\DC1\255\255\255\255\255\255\255\254\ETX\SOH\ETXabs\EOT\b\n"
        , TestCase $ assertEqual "defines" (transpileIntoBytecode [Push (Float (-5.2)), DefineEnv "a" Define Nothing, DefineEnv "a" Redefine (Just (Lambda 1 [PushArg 0, Push (Bool False), Push (Symbol (Operator Neq)), Call, Ret]))]) "db4\n\SOH\DC2R\SOH\a\DC4\204\204\204\204\204\205\255\255\255\255\255\255\255\206\ENQ\SOH\SOHaAS\ENQ\SOH\SOHaBQ\SYN\SOH\SOH\SOH\f\STX\SOH\NUL\SOH\DC3\NUL\SOH\NAK!8\EOT\b\n"
        , TestCase $ assertEqual "jump too long (should not raise)" (transpileIntoBytecode [Push (Bool True), Jump 46 (Just True), Ret]) "db4\n\SOH\DC3\SOH\a\NUL\NUL\NUL\NUL\NUL\NUL\NUL.Q\b\n"
        , TestCase $ assertEqual "invalid variables for op (should not raise)" (transpileIntoBytecode [Push Void, Push (String "bonsoir a tous"), Push (Bool True), Push (Symbol (Operator Eq)), Call, Ret]) "db4\n\SOH\ETB\SOH\DC4\SOH\SObonsoir a tous\SOH\DC3\SOH\SOH\NAK!7\EOT\b\n"
        , TestCase $ assertEqual "invalid redefine (should not raise)" (transpileIntoBytecode [DefineEnv "b" Define Nothing, DefineEnv "a" Redefine (Just (String "haha")), Ret]) "db4\n\ENQ\SOH\SOHbAS\ENQ\SOH\SOHaBQ\DC4\SOH\EOThaha\b\n"
        ]

testEncodeDecode :: Test
testEncodeDecode =
    TestList
        [ TestCase $ assertEqual "basic compilation" (getFromBytecode (transpileIntoBytecode [Push (Integer 1), Ret])) (Right [Push (Integer 1), Ret])
        , TestCase $ assertEqual "function" (getFromBytecode (transpileIntoBytecode [DefineEnv "abs" Define (Just (Lambda 1 [PushArg 0, Push (Integer (-1))])), Ret])) (Right [DefineEnv "abs" Define (Just (Lambda 1 [PushArg 0, Push (Integer (-1))])), Ret])
        , TestCase $ assertEqual "operators with floats" (getFromBytecode (transpileIntoBytecode [Push (Float 1.12), Push (Float (-2.47)), Push (Symbol (Operator Mul)), Call, PushEnv "print"])) (Right [Push (Float 1.12), Push (Float (-2.47)), Push (Symbol (Operator Mul)), Call, PushEnv "print"])
        , TestCase $ assertEqual "defines with variables" (getFromBytecode (transpileIntoBytecode [Push (Bool True), DefineEnv "flt" Override Nothing, DefineEnv "cop" Redefine (Just (String "yes"))])) (Right [Push (Bool True), DefineEnv "flt" Override Nothing, DefineEnv "cop" Redefine (Just (String "yes"))])
        , TestCase $ assertEqual "define with jump" (getFromBytecode (transpileIntoBytecode [DefineEnv "b" Define (Just (Bool False)), Jump (-1) (Just True), Ret])) (Right [DefineEnv "b" Define (Just (Bool False)), Jump (-1) (Just True), Ret])
        , TestCase $ assertEqual "define and erase with jumps" (getFromBytecode (transpileIntoBytecode [Jump 0 (Just False), DefineEnv "b" Define (Just (Bool False)), Jump 1 Nothing, EraseEnv "b", PushEnv "b", Ret])) (Right [Jump 0 (Just False), DefineEnv "b" Define (Just (Bool False)), Jump 1 Nothing, EraseEnv "b", PushEnv "b", Ret])
        ]

testDecodeErrorHandling :: Test
testDecodeErrorHandling =
    TestList
        [ TestCase $ assertEqual "wrong header" (getFromBytecode "db5\n\SOH\DC2Q\x01\a\FS\NUL\NUL\NUL\NUL\NUL\NUL\255\255\255\255\255\255\255\206\n") (Left "Exec format error")
        , TestCase $ assertEqual "wrong integer size (inf)" (getFromBytecode "db4\n\SOH\DC2Q\x01\a\FS\NUL\NUL\NUL\NUL\NUL\NUL\255\255\255\255\255") (Left "Not enough space for an int")
        , TestCase $ assertEqual "wrong integer size (code)" (getFromBytecode "db4\n\SOH\DC2Q\x01\a\FS\NUL\NUL\NUL\NUL") (Left "Not enough space for an integer")
        , TestCase $ assertEqual "wrong size" (getFromBytecode "db4\n\SOH\DC2Q\x02\NUL") (Left "Not enough space for a size")
        , TestCase $ assertEqual "wrong int size" (getFromBytecode "db4\n\SOH\DC4\x01\ENQHell\n") (Left "Wrong string length")
        ]
