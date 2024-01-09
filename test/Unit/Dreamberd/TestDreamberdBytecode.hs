module Unit.Dreamberd.TestDreamberdBytecode (testDreamberdBytecode) where

import Dreamberd.Bytecode.Encode (transpileCall, transpileInstruction, transpileInt, transpileIntoBytecode, transpileString)
import Dreamberd.Vm
import Test.HUnit (Test (..), assertEqual)

testDreamberdBytecode :: Test
testDreamberdBytecode = TestList [testTranspileInt, testTranspileString, testTranspileCall, testTranspileInstruction, testTranspileIntoBytecode]

testTranspileInt :: Test
testTranspileInt =
    TestLabel "transpileInt" $
        TestList
            [ TestCase $ assertEqual "Transpile Int" (transpileInt 1) "\x00\x00\x00\x01"
            , TestCase $ assertEqual "Transpile Int Zero" (transpileInt 0) "\x00\x00\x00\x00"
            , TestCase $ assertEqual "Transpile Int Negative" (transpileInt (-1)) "\xFF\xFF\xFF\xFF"
            ]

testTranspileString :: Test
testTranspileString =
    TestLabel "transpileString" $
        TestList
            [ TestCase $ assertEqual "Transpile String" (transpileString "Hello") "\x00\x00\x00\x05Hello"
            , TestCase $ assertEqual "Transpile String2" (transpileString "World") "\x00\x00\x00\x05World"
            , TestCase $ assertEqual "Transpile String3" (transpileString "Hello World") "\x00\x00\x00\x0BHello World"
            , TestCase $ assertEqual "Transpile String4" (transpileString "Hello\nWorld") "\x00\x00\x00\x0BHello\nWorld"
            , TestCase $ assertEqual "Transpile String5" (transpileString "Hello\nWorld\n") "\x00\x00\x00\x0CHello\nWorld\n"
            , TestCase $ assertEqual "Transpile String6" (transpileString "Hello\nWorld\n\n") "\x00\x00\x00\x0DHello\nWorld\n\n"
            , TestCase $ assertEqual "Transpile String7" (transpileString "Hello\nWorld\n\n\n") "\x00\x00\x00\x0EHello\nWorld\n\n\n"
            , TestCase $ assertEqual "Transpile Empty String" (transpileString "") "\x00\x00\x00\x00"
            ]

testTranspileCall :: Test
testTranspileCall =
    TestLabel "transpileCall" $
        TestList
            [ TestCase $ assertEqual "Transpile Call Add" (transpileCall Add) "\x21"
            , TestCase $ assertEqual "Transpile Call Sub" (transpileCall Sub) "\x22"
            , TestCase $ assertEqual "Transpile Call Mul" (transpileCall Mul) "\x23"
            , TestCase $ assertEqual "Transpile Call Div" (transpileCall Div) "\x24"
            , TestCase $ assertEqual "Transpile Call Eq" (transpileCall Eq) "\x25"
            , TestCase $ assertEqual "Transpile Call Neq" (transpileCall Neq) "\x26"
            , TestCase $ assertEqual "Transpile Call Less" (transpileCall Less) "\x27"
            , TestCase $ assertEqual "Transpile Call LessOrEqual" (transpileCall LessOrEqual) "\x28"
            , TestCase $ assertEqual "Transpile Call Greater" (transpileCall Greater) "\x29"
            , TestCase $ assertEqual "Transpile Call GreaterOrEqual" (transpileCall GreaterOrEqual) "\x2A"
            , TestCase $ assertEqual "Transpile Call FunctionName" (transpileCall (FunctionName "test")) "\x2B\x00\x00\x00\x04test"
            ]

testTranspileInstruction :: Test
testTranspileInstruction =
    TestLabel "transpileInstruction" $
        TestList
            [ TestCase $ assertEqual "Transpile Instruction Push" (transpileInstruction (Push (Number 1))) "\x01\x11\x00\x00\x00\x01"
            , TestCase $ assertEqual "Transpile Instruction PushArg" (transpileInstruction (PushArg 1)) "\x02\x00\x00\x00\x01"
            , TestCase $ assertEqual "Transpile Instruction PushEnv" (transpileInstruction (PushEnv "test")) "\x03\x00\x00\x00\x04test"
            , TestCase $ assertEqual "Transpile Instruction Call" (transpileInstruction Call) "\x04"
            , TestCase $ assertEqual "Transpile Instruction DefineEnv" (transpileInstruction (DefineEnv "test" (Function []))) "\x05\x00\x00\x00\x04test\x31\x00\x00\x00\x00"
            , TestCase $ assertEqual "Transpile Instruction DefineEnvFromStack" (transpileInstruction (DefineEnvFromStack "test")) "\x06\x00\x00\x00\x04test"
            , TestCase $ assertEqual "Transpile Instruction JumpIfFalse" (transpileInstruction (JumpIfFalse 1)) "\x07\x00\x00\x00\x01"
            , TestCase $ assertEqual "Transpile Instruction Ret" (transpileInstruction Ret) "\x08"
            ]

testTranspileIntoBytecode :: Test
testTranspileIntoBytecode =
    TestLabel "transpileBytecode" $
        TestList
            [ TestCase $ assertEqual "Transpile Into Bytecode" (transpileIntoBytecode [Push (Number 1), Ret]) "db4\n\x01\x11\x00\x00\x00\x01\x08\n"
            ]
