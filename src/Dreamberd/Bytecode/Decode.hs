module Dreamberd.Bytecode.Decode (
    getFromBytecode,
) where

import Data.Bits (shiftL, (.|.))
import Data.Char (ord)
import Dreamberd.Vm (Call (..), DefineEnvType (..), Insts (..), Value (..))

parseSize :: [Char] -> Either String (Int, [Char])
parseSize (size : bytes) | length bytes >= sized = Right (parseUnsignedInt val sized, rest)
  where
    sized = fromEnum size
    (val, rest) = splitAt sized bytes
parseSize _ = Left "Not enough space for a size"

parseUnsignedInt :: [Char] -> Int -> Int
parseUnsignedInt [] _ = 0
parseUnsignedInt (x:xs) idx = max (256 ^ (idx - 1)) 1 * fromEnum x + parseUnsignedInt xs (idx - 1)

parseInt :: [Char] -> Either String (Int, [Char])
parseInt (b1 : b2 : b3 : b4 : b5 : b6 : b7 : b8 : rest) = Right ((ord b1 `shiftL` 56) .|. (ord b2 `shiftL` 48) .|. (ord b3 `shiftL` 40) .|. (ord b4 `shiftL` 32) .|. (ord b5 `shiftL` 24) .|. (ord b6 `shiftL` 16) .|. (ord b7 `shiftL` 8) .|. ord b8, rest)
parseInt _ = Left "Not enough space for an int"

parseInteger' :: Int -> [Char] -> Either String (Integer, [Char])
parseInteger' 0 bytes = Right (0, bytes)
parseInteger' size (b : bytes) = parseInteger' (size - 1) bytes >>= \(val, rest) -> Right (fromIntegral (fromEnum b) * (256 ^ (size - 1)) + val, rest)
parseInteger' _ _ = Left "Not enough space for an integer"

parseInteger :: [Char] -> Either String (Integer, [Char])
parseInteger (c : bytes)
    | fromEnum c == 0x51 = parseSize bytes >>= uncurry parseInteger'
    | fromEnum c == 0x52 = parseSize bytes >>= \(size, rest) -> parseInteger' size rest >>= \(val, rest') -> Right (-val, rest')
parseInteger _ = Left "Not enough space for an integer"

parseString :: [Char] -> Either String (String, [Char])
parseString bytes =
    parseSize bytes >>= \(val, rest) ->
        if length rest < val then Left "Wrong string length" else Right (splitAt val rest)

parseCall :: [Char] -> Either String (Call, [Char])
parseCall [] = Left "No call provided"
parseCall (c : op : bytes)
    | fromEnum c == 0x21 && hex >= 0x31 && hex <= 0x3F = Right (Operator (toEnum $ hex - 0x31), bytes)
    | fromEnum c == 0x22 && hex >= 0x25 && hex <= 0x27 = Right (Builtin (toEnum $ hex - 0x25), bytes)
  where
    hex = fromEnum op
parseCall _ = Left "Unknown call"

parseValue :: [Char] -> Either String (Value, [Char])
parseValue [] = Left "No value provided"
parseValue (c : val : bytes) | fromEnum c == 0x13 = case fromEnum val of
    0 -> Right (Bool False, bytes)
    1 -> Right (Bool True, bytes)
    _ -> Left "Unknown value for type Bool"
parseValue (c : bytes) = case fromEnum c of
    0x11 -> parseInt bytes >>= \(val, rest) -> Right (Integer val, rest)
    0x12 -> parseInteger bytes >>= \(l, rest) -> parseInt rest >>= \(r, rest') -> Right (Float (encodeFloat l r), rest')
    0x14 -> parseString bytes >>= \(val, rest) -> Right (String val, rest)
    0x15 -> parseCall bytes >>= \(val, rest) -> Right (Symbol val, rest)
    0x16 ->
        parseSize bytes >>= \(args, rest) ->
            parseSize rest >>= \(len, rest') ->
                let (insts, rest'') = splitAt len rest' in parseInstructions insts >>= \func -> if length rest' < len then Left "Wrong lambda body length" else Right (Lambda args func, rest'')
    0x17 -> Right (Void, bytes)
    _ -> Left "Unknown value type"

parseDefineValue :: [Char] -> Either String (Maybe Value, [Char])
parseDefineValue [] = Left "No value provided"
parseDefineValue (c : bytes)
    | fromEnum c == 0x51 = parseValue bytes >>= \(val, rest) -> Right (Just val, rest)
    | fromEnum c == 0x53 = Right (Nothing, bytes)
    | otherwise = Left "Unknown value type"

parseDefineEnv :: String -> [Char] -> Either String (Insts, [Char])
parseDefineEnv _ [] = Left "No value provided"
parseDefineEnv name (c : bytes) = case fromEnum c of
    0x41 -> parseDefineValue bytes >>= \(val, rest) -> Right (DefineEnv name Define val, rest)
    0x42 -> parseDefineValue bytes >>= \(val, rest) -> Right (DefineEnv name Redefine val, rest)
    0x43 -> parseDefineValue bytes >>= \(val, rest) -> Right (DefineEnv name Override val, rest)
    _ -> Left "Unknown define env"

parseJump :: (Int, [Char]) -> Either String [Insts]
parseJump (val, c : bytes) = case fromEnum c of
    0x51 -> pursueParsing (Jump val (Just True)) bytes
    0x52 -> pursueParsing (Jump val (Just False)) bytes
    0x53 -> pursueParsing (Jump val Nothing) bytes
    _ -> Left "Unknown jump"
parseJump _ = Left "Wrong jump length"

parseInstructions :: [Char] -> Either String [Insts]
parseInstructions [] = Right []
parseInstructions (c : bytes) = case fromEnum c of
    0x01 -> parseValue bytes >>= \(val, rest) -> pursueParsing (Push val) rest
    0x02 -> parseSize bytes >>= \(val, rest) -> pursueParsing (PushArg val) rest
    0x03 -> parseString bytes >>= \(val, rest) -> pursueParsing (PushEnv val) rest
    0x04 -> pursueParsing Call bytes
    0x05 -> parseString bytes >>= uncurry parseDefineEnv >>= uncurry pursueParsing
    0x06 -> parseString bytes >>= \(name, rest) -> pursueParsing (EraseEnv name) rest
    0x07 -> parseInt bytes >>= parseJump
    0x08 -> pursueParsing Ret bytes
    _ -> Left "Unknown instruction"

pursueParsing :: Insts -> [Char] -> Either String [Insts]
pursueParsing inst rest = parseInstructions rest >>= \insts -> Right (inst : insts)

getFromBytecode :: [Char] -> Either String [Insts]
getFromBytecode ('d' : 'b' : '4' : '\n' : bytes) = parseInstructions $ init bytes
getFromBytecode _ = Left "Exec format error"
