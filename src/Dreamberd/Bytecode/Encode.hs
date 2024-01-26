module Dreamberd.Bytecode.Encode (
    transpileCall,
    transpileInt,
    transpileInteger,
    transpileSize,
    transpileString,
    transpileValue,
    transpileInstruction,
    transpileIntoBytecode,
) where

import Data.Bits (shiftR)
import Data.Char (chr)
import Dreamberd.Vm (Call (..), DefineEnvType (..), Insts (..), Value (..))

getIntegerSize :: Integer -> Int
getIntegerSize nb = if nb == 0 then 0 else 1 + getIntegerSize (nb `shiftR` 8)

transpileSize :: Int -> [Char]
transpileSize nb = toEnum size : transpileInt nb size
  where
    size = getNumberSize nb 0

getNumberSize :: Int -> Int -> Int
getNumberSize nb idx
    | nb < 256 = idx + 1
    | otherwise = getNumberSize (nb `shiftR` 8) (idx + 1)

transpileInt :: Int -> Int -> [Char]
transpileInt nb size = map chr $ reverse $ map (`mod` 256) $ take size $ iterate (`shiftR` 8) nb

transpileInteger :: Integer -> [Char]
transpileInteger nb =
    (if nb >= 0 then toEnum 0x51 else toEnum 0x52) : transpileSize size ++ map chr (reverse $ map (`mod` 256) $ take size $ iterate (`shiftR` 8) (fromInteger (abs nb)))
  where
    size = getIntegerSize (abs nb)

transpileString :: String -> [Char]
transpileString str = transpileSize (length str) ++ str

transpileCall :: Call -> [Char]
transpileCall (Operator op) = toEnum 0x21 : [toEnum (0x31 + fromEnum op)]
transpileCall (Builtin builtin) = toEnum 0x22 : [toEnum (0x25 + fromEnum builtin)]

transpileValue :: Value -> [Char]
transpileValue (Integer nb) = toEnum 0x11 : transpileInt nb 8
transpileValue (Float nb) = toEnum 0x12 : transpileInteger l ++ transpileInt r 8
  where
    (l, r) = decodeFloat nb
transpileValue (Bool b) = toEnum 0x13 : (if b then [toEnum 1] else [toEnum 0])
transpileValue (String str) = toEnum 0x14 : transpileString str
transpileValue (Symbol call) = toEnum 0x15 : transpileCall call
transpileValue (Lambda args insts) = toEnum 0x16 : transpileSize args ++ transpileSize (length nested) ++ nested
  where
    nested = foldMap transpileInstruction insts
transpileValue Void = [toEnum 0x17]

transpileDefineValue :: Maybe Value -> [Char]
transpileDefineValue Nothing = [toEnum 0x53]
transpileDefineValue (Just val) = toEnum 0x51 : transpileValue val

transpileInstruction :: Insts -> [Char]
transpileInstruction (Push v) = toEnum 0x01 : transpileValue v
transpileInstruction (PushArg idx) = toEnum 0x02 : transpileSize idx
transpileInstruction (PushEnv env) = toEnum 0x03 : transpileString env
transpileInstruction Call = [toEnum 0x04]
transpileInstruction (DefineEnv name Define value) = toEnum 0x05 : transpileString name ++ [toEnum 0x41] ++ transpileDefineValue value
transpileInstruction (DefineEnv name Redefine value) = toEnum 0x05 : transpileString name ++ [toEnum 0x42] ++ transpileDefineValue value
transpileInstruction (DefineEnv name Override value) = toEnum 0x05 : transpileString name ++ [toEnum 0x43] ++ transpileDefineValue value
transpileInstruction (EraseEnv name) = toEnum 0x06 : transpileString name
transpileInstruction (Jump nb (Just True)) = toEnum 0x07 : transpileInt nb 8 ++ [toEnum 0x51]
transpileInstruction (Jump nb (Just False)) = toEnum 0x07 : transpileInt nb 8 ++ [toEnum 0x52]
transpileInstruction (Jump nb Nothing) = toEnum 0x07 : transpileInt nb 8 ++ [toEnum 0x53]
transpileInstruction Ret = [toEnum 0x08]

transpileIntoBytecode :: [Insts] -> [Char]
transpileIntoBytecode insts = "db4\n" ++ foldMap transpileInstruction insts ++ "\n"
