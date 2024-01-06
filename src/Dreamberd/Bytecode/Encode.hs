module Dreamberd.Bytecode.Encode (
    transpileIntoBytecode,
) where

import Data.Bits (shiftR)
import Data.Char (chr)
import Dreamberd.Vm (Call(..), EnvValue(..), Insts(..), Value(..))
import System.Endian (getSystemEndianness, Endianness(..))

transpileInt :: Int -> [Char]
transpileInt nb = case getSystemEndianness of
    LittleEndian -> map chr $ reverse $ map (`mod` 256) $ take 4 $ iterate (`shiftR` 8) nb
    BigEndian -> map (chr . (`mod` 256)) (take 4 $ iterate (`shiftR` 8) nb)

transpileString :: String -> [Char]
transpileString str = transpileInt (length str) ++ str

transpileCall :: Call -> [Char]
transpileCall Add = [toEnum 0x20]
transpileCall Sub = [toEnum 0x21]
transpileCall Mul = [toEnum 0x22]
transpileCall Div = [toEnum 0x23]
transpileCall Eq = [toEnum 0x24]
transpileCall Neq = [toEnum 0x25]
transpileCall Less = [toEnum 0x26]
transpileCall LessOrEqual = [toEnum 0x27]
transpileCall Greater = [toEnum 0x28]
transpileCall GreaterOrEqual = [toEnum 0x29]
transpileCall (FunctionName str) = toEnum 0x2A : transpileString str

transpileValue :: Value -> [Char]
transpileValue (Number nb) = toEnum 0x11 : transpileInt nb
transpileValue (Bool b) = toEnum 0x12 : (if b then [toEnum 1] else [toEnum 0])
transpileValue (Symbol call) = toEnum 0x13 : transpileCall call
transpileValue Void = [toEnum 0x14]

transpileInstruction :: Insts -> [Char]
transpileInstruction (Push v) = toEnum 0x01 : transpileValue v
transpileInstruction (PushArg idx) = toEnum 0x02 : transpileInt idx
transpileInstruction (PushEnv env) = toEnum 0x03 : transpileString env
transpileInstruction Call = [toEnum 0x04]
transpileInstruction (DefineEnv name value) = toEnum 0x05 : transpileString name ++ case value of
    (Function insts) -> toEnum 0x30 : transpileInt (length nested) ++ nested
        where nested = foldMap transpileInstruction insts
    (Variable val) -> toEnum 0x31 : transpileValue val
transpileInstruction (JumpIfFalse nb) = toEnum 0x06 : transpileInt nb
transpileInstruction Ret = [toEnum 0x07]

transpileIntoBytecode :: [Insts] -> [Char]
transpileIntoBytecode insts = "db4\n" ++ foldMap transpileInstruction insts
