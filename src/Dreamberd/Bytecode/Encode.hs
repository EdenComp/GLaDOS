module Dreamberd.Bytecode.Encode (
    transpileIntoBytecode,
) where

import Data.Bits (shiftR)
import Data.Char (chr)
import Dreamberd.Vm (Call (..), EnvValue (..), Insts (..), Value (..))
import System.Endian (Endianness (..), getSystemEndianness)

transpileInt :: Int -> [Char]
transpileInt nb = case getSystemEndianness of
    LittleEndian -> map chr $ reverse $ map (`mod` 256) $ take 4 $ iterate (`shiftR` 8) nb
    BigEndian -> map (chr . (`mod` 256)) (take 4 $ iterate (`shiftR` 8) nb)

transpileString :: String -> [Char]
transpileString str = transpileInt (length str) ++ str

transpileCall :: Call -> [Char]
transpileCall Add = [toEnum 0x21]
transpileCall Sub = [toEnum 0x22]
transpileCall Mul = [toEnum 0x23]
transpileCall Div = [toEnum 0x24]
transpileCall Eq = [toEnum 0x25]
transpileCall Neq = [toEnum 0x26]
transpileCall Less = [toEnum 0x27]
transpileCall LessOrEqual = [toEnum 0x28]
transpileCall Greater = [toEnum 0x29]
transpileCall GreaterOrEqual = [toEnum 0x2A]
transpileCall (FunctionName str) = toEnum 0x2B : transpileString str

transpileValue :: Value -> [Char]
transpileValue (Number nb) = toEnum 0x11 : transpileInt nb
transpileValue (Bool b) = toEnum 0x12 : (if b then [toEnum 1] else [toEnum 0])
transpileValue (String str) = toEnum 0x13 : transpileString str
transpileValue (Symbol call) = toEnum 0x14 : transpileCall call
transpileValue Void = [toEnum 0x15]

transpileInstruction :: Insts -> [Char]
transpileInstruction (Push v) = toEnum 0x01 : transpileValue v
transpileInstruction (PushArg idx) = toEnum 0x02 : transpileInt idx
transpileInstruction (PushEnv env) = toEnum 0x03 : transpileString env
transpileInstruction Call = [toEnum 0x04]
transpileInstruction (DefineEnv name value) =
    toEnum 0x05 : transpileString name ++ case value of
        (Function insts) -> toEnum 0x31 : transpileInt (length nested) ++ nested
          where
            nested = foldMap transpileInstruction insts
        (Variable val) -> toEnum 0x32 : transpileValue val
transpileInstruction (DefineEnvFromStack name) = toEnum 0x06 : transpileString name
transpileInstruction (Jump nb) = toEnum 0x07 : transpileInt nb
transpileInstruction (JumpIfFalse nb) = toEnum 0x08 : transpileInt nb
transpileInstruction Ret = [toEnum 0x09]

transpileIntoBytecode :: [Insts] -> [Char]
transpileIntoBytecode insts = "db4\n" ++ foldMap transpileInstruction insts ++ "\n"
