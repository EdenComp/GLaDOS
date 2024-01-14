module Dreamberd.Bytecode.Decode (
    getFromBytecode,
) where

import Data.Bits (shiftL, (.|.))
import Data.Char (ord)
import Dreamberd.Vm (Call (..), DefineEnvType (..), EnvValue (..), Insts (..), Value (..))

parseInt :: [Char] -> Either String (Int, [Char])
parseInt (b1 : b2 : b3 : b4 : b5 : b6 : b7 : b8 : rest) = Right ((ord b1 `shiftL` 56) .|. (ord b2 `shiftL` 48) .|. (ord b3 `shiftL` 40) .|. (ord b4 `shiftL` 32) .|. (ord b5 `shiftL` 24) .|. (ord b6 `shiftL` 16) .|. (ord b7 `shiftL` 8) .|. ord b8, rest)
parseInt _ = Left "Not enough space for an int"

parseInteger' :: Int -> [Char] -> Either String (Integer, [Char])
parseInteger' 0 bytes = Right (0, bytes)
parseInteger' size (b : bytes) = parseInteger' (size - 1) bytes >>= \(val, rest) -> Right (fromIntegral (fromEnum b) * (256 ^ (size - 1)) + val, rest)
parseInteger' _ _ = Left "Not enough space for an integer"

parseInteger :: [Char] -> Either String (Integer, [Char])
parseInteger (c : bytes)
    | fromEnum c == 0x51 = parseInt bytes >>= uncurry parseInteger'
    | fromEnum c == 0x52 = parseInt bytes >>= \(size, rest) -> parseInteger' size rest >>= \(val, rest') -> Right (-val, rest')
parseInteger _ = Left "Not enough space for an integer"

parseString :: [Char] -> Either String (String, [Char])
parseString bytes =
    parseInt bytes >>= \(val, rest) ->
        if length rest < val then Left "Wrong string length" else Right (splitAt val rest)

parseCall :: [Char] -> Either String (Call, [Char])
parseCall [] = Left "No call provided"
parseCall (c : op : bytes) | fromEnum c == 0x21 && hex >= 0x31 && hex <= 0x3F = Right (Builtin (toEnum $ hex - 0x31), bytes)
  where
    hex = fromEnum op
parseCall (c : bytes)
    | fromEnum c == 0x22 = parseString bytes >>= \(name, rest) -> Right (FunctionName name, rest)
    | otherwise = Left "Unknown call"

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
    0x16 -> Right (Void, bytes)
    _ -> Left "Unknown value type"

parseEnvValue :: [Char] -> Either String (Maybe EnvValue, [Char])
parseEnvValue [] = Left "No value provided"
parseEnvValue (c : bytes)
    | fromEnum c == 0x41 =
        parseInt bytes >>= \(len, rest) ->
            let (insts, rest') = splitAt len rest in parseInstructions insts >>= \func -> if length rest < len then Left "Wrong function body length" else Right (Just (Function func), rest')
    | fromEnum c == 0x42 = parseValue bytes >>= \(val, rest) -> Right (Just (Variable val), rest)
    | fromEnum c == 0x53 = Right (Nothing, bytes)
    | otherwise = Left "Unknown value type"

parseDefineEnv :: String -> [Char] -> Either String (Insts, [Char])
parseDefineEnv _ [] = Left "No value provided"
parseDefineEnv name (c : bytes) = case fromEnum c of
    0x45 -> parseEnvValue bytes >>= \(val, rest) -> Right (DefineEnv name Define val, rest)
    0x46 -> parseEnvValue bytes >>= \(val, rest) -> Right (DefineEnv name Redefine val, rest)
    0x47 -> parseEnvValue bytes >>= \(val, rest) -> Right (DefineEnv name Override val, rest)
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
    0x02 -> parseInt bytes >>= \(val, rest) -> pursueParsing (PushArg val) rest
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
