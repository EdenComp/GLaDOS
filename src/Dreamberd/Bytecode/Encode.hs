module Dreamberd.Bytecode.Encode (
    transpileCall,
    transpileInt,
    transpileString,
    transpileValue,
    transpileInstruction,
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
transpileCall (Builtin op) = toEnum 0x21 : [toEnum (0x31 + fromEnum op)]
transpileCall (FunctionName str) = toEnum 0x22 : transpileString str

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
        (Function insts) -> toEnum 0x41 : transpileInt (length nested) ++ nested
          where
            nested = foldMap transpileInstruction insts
        (Variable val) -> toEnum 0x42 : transpileValue val
transpileInstruction (RedefineEnv name value) =
    toEnum 0x09 : transpileString name ++ case value of
        (Function insts) -> toEnum 0x41 : transpileInt (length nested) ++ nested
          where
            nested = foldMap transpileInstruction insts
        (Variable val) -> toEnum 0x42 : transpileValue val
transpileInstruction (DefineEnvFromStack name) = toEnum 0x06 : transpileString name
transpileInstruction (RedefineEnvFromStack name) = toEnum 0x0A : transpileString name
transpileInstruction (EraseEnv name) = toEnum 0x0B : transpileString name
transpileInstruction (Jump nb (Just True)) = toEnum 0x07 : transpileInt nb ++ [toEnum 0x51]
transpileInstruction (Jump nb (Just False)) = toEnum 0x07 : transpileInt nb ++ [toEnum 0x52]
transpileInstruction (Jump nb Nothing) = toEnum 0x07 : transpileInt nb ++ [toEnum 0x53]
transpileInstruction Ret = [toEnum 0x08]

transpileIntoBytecode :: [Insts] -> [Char]
transpileIntoBytecode insts = "db4\n" ++ foldMap transpileInstruction insts ++ "\n"
