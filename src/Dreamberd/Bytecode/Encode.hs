module Dreamberd.Bytecode.Encode (
    transpileCall,
    transpileInt,
    transpileInteger,
    transpileString,
    transpileValue,
    transpileInstruction,
    transpileIntoBytecode,
) where

import Data.Bits (shiftR)
import Data.Char (chr)
import Dreamberd.Vm (Call (..), DefineEnvType (..), EnvValue (..), Insts (..), Value (..))
import System.Endian (Endianness (..), getSystemEndianness)

getIntegerSize :: Integer -> Int
getIntegerSize nb = if nb == 0 then 0 else 1 + getIntegerSize (nb `shiftR` 8)

transpileInt :: Int -> [Char]
transpileInt nb = case getSystemEndianness of
    LittleEndian -> map chr $ reverse $ map (`mod` 256) $ take 8 $ iterate (`shiftR` 8) nb
    BigEndian -> map (chr . (`mod` 256)) (take 8 $ iterate (`shiftR` 8) nb)

transpileInteger :: Integer -> [Char]
transpileInteger nb =
    (if nb >= 0 then toEnum 0x51 else toEnum 0x52) : transpileInt size ++ case getSystemEndianness of
        LittleEndian -> map chr $ reverse $ map (`mod` 256) $ take size $ iterate (`shiftR` 8) (fromInteger (abs nb))
        BigEndian -> map (chr . (`mod` 256)) (take size $ iterate (`shiftR` 8) (fromInteger (abs nb)))
  where
    size = getIntegerSize (abs nb)

transpileString :: String -> [Char]
transpileString str = transpileInt (length str) ++ str

transpileCall :: Call -> [Char]
transpileCall (Builtin op) = toEnum 0x21 : [toEnum (0x31 + fromEnum op)]
transpileCall (FunctionName str) = toEnum 0x22 : transpileString str

transpileValue :: Value -> [Char]
transpileValue (Integer nb) = toEnum 0x11 : transpileInt nb
transpileValue (Float nb) = toEnum 0x12 : transpileInteger l ++ transpileInt r
  where
    (l, r) = decodeFloat nb
transpileValue (Bool b) = toEnum 0x13 : (if b then [toEnum 1] else [toEnum 0])
transpileValue (String str) = toEnum 0x14 : transpileString str
transpileValue (Symbol call) = toEnum 0x15 : transpileCall call
transpileValue (Lambda args insts) = toEnum 0x16 : transpileInt args ++ transpileInt (length nested) ++ nested
  where
    nested = foldMap transpileInstruction insts
transpileValue Void = [toEnum 0x17]

transpileEnvValue :: Maybe EnvValue -> [Char]
transpileEnvValue Nothing = [toEnum 0x53]
transpileEnvValue (Just (Function args insts)) = toEnum 0x41 : transpileInt args ++ transpileInt (length nested) ++ nested
  where
    nested = foldMap transpileInstruction insts
transpileEnvValue (Just (Variable val)) = toEnum 0x42 : transpileValue val

transpileInstruction :: Insts -> [Char]
transpileInstruction (Push v) = toEnum 0x01 : transpileValue v
transpileInstruction (PushArg idx) = toEnum 0x02 : transpileInt idx
transpileInstruction (PushEnv env) = toEnum 0x03 : transpileString env
transpileInstruction Call = [toEnum 0x04]
transpileInstruction (DefineEnv name Define value) = toEnum 0x05 : transpileString name ++ [toEnum 0x45] ++ transpileEnvValue value
transpileInstruction (DefineEnv name Redefine value) = toEnum 0x05 : transpileString name ++ [toEnum 0x46] ++ transpileEnvValue value
transpileInstruction (DefineEnv name Override value) = toEnum 0x05 : transpileString name ++ [toEnum 0x47] ++ transpileEnvValue value
transpileInstruction (EraseEnv name) = toEnum 0x06 : transpileString name
transpileInstruction (Jump nb (Just True)) = toEnum 0x07 : transpileInt nb ++ [toEnum 0x51]
transpileInstruction (Jump nb (Just False)) = toEnum 0x07 : transpileInt nb ++ [toEnum 0x52]
transpileInstruction (Jump nb Nothing) = toEnum 0x07 : transpileInt nb ++ [toEnum 0x53]
transpileInstruction Ret = [toEnum 0x08]

transpileIntoBytecode :: [Insts] -> [Char]
transpileIntoBytecode insts = "db4\n" ++ foldMap transpileInstruction insts ++ "\n"
