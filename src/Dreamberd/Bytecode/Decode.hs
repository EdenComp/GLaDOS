module Dreamberd.Bytecode.Decode (
    getFromBytecode,
) where

import Dreamberd.Vm (Call (..), EnvValue (..), Insts (..), Value (..))

parseInt :: [Char] -> Either String (Int, [Char])
parseInt (b1 : b2 : b3 : b4 : rest) = Right (val, rest)
  where
    val = fromEnum b1 * 256 * 256 * 256 + fromEnum b2 * 256 * 256 + fromEnum b3 * 256 + fromEnum b4
parseInt _ = Left "Not enough space for an int"

parseString :: [Char] -> Either String (String, [Char])
parseString bytes =
    parseInt bytes >>= \(val, rest) ->
        if length rest < val then Left "Wrong string length" else Right (splitAt val rest)

parseCall :: [Char] -> Either String (Call, [Char])
parseCall [] = Left "No call provided"
parseCall (c : op : bytes) | fromEnum c == 0x21 && hex >= 0x31 && hex <= 0x3B = Right (Builtin (toEnum $ hex - 0x31), bytes)
  where
    hex = fromEnum op
parseCall (c : bytes)
    | fromEnum c == 0x22 = parseString bytes >>= \(name, rest) -> Right (FunctionName name, rest)
    | otherwise = Left "Unknown call"

parseValue :: [Char] -> Either String (Value, [Char])
parseValue [] = Left "No value provided"
parseValue (c : val : bytes) | fromEnum c == 0x12 = case fromEnum val of
    0 -> Right (Bool False, bytes)
    1 -> Right (Bool True, bytes)
    _ -> Left "Unknown value for type Bool"
parseValue (c : bytes) = case fromEnum c of
    0x11 -> parseInt bytes >>= \(val, rest) -> Right (Number val, rest)
    0x13 -> parseString bytes >>= \(val, rest) -> Right (String val, rest)
    0x14 -> parseCall bytes >>= \(val, rest) -> Right (Symbol val, rest)
    0x15 -> Right (Void, bytes)
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
    0x51 -> parseEnvValue bytes >>= \(val, rest) -> Right (DefineEnv name True val, rest)
    0x52 -> parseEnvValue bytes >>= \(val, rest) -> Right (DefineEnv name False val, rest)
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
