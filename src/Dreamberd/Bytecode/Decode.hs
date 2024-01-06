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
parseString bytes = case parseInt bytes of
    Left err -> Left err
    Right (val, rest) -> if length rest < val then Left "Wrong string length" else Right (str, rest')
      where
        (str, rest') = splitAt val rest

parseCall :: [Char] -> Either String (Call, [Char])
parseCall [] = Left "No call provided"
parseCall (c : bytes) = case fromEnum c of
    0x20 -> Right (Add, bytes)
    0x21 -> Right (Sub, bytes)
    0x22 -> Right (Mul, bytes)
    0x23 -> Right (Div, bytes)
    0x24 -> Right (Eq, bytes)
    0x25 -> Right (Neq, bytes)
    0x26 -> Right (Less, bytes)
    0x27 -> Right (LessOrEqual, bytes)
    0x28 -> Right (Greater, bytes)
    0x29 -> Right (GreaterOrEqual, bytes)
    0x2A -> case parseString bytes of
        Left err -> Left err
        Right (name, rest) -> Right (FunctionName name, rest)
    _ -> Left "Unknown call"

parseValue :: [Char] -> Either String (Value, [Char])
parseValue [] = Left "No value provided"
parseValue (c : val : bytes) | fromEnum c == 0x12 = case fromEnum val of
    0 -> Right (Bool False, bytes)
    1 -> Right (Bool True, bytes)
    _ -> Left "Unknown value for type Bool"
parseValue (c : bytes) = case fromEnum c of
    0x11 -> case parseInt bytes of
        Left err -> Left err
        Right (val, rest) -> Right (Number val, rest)
    0x13 -> case parseCall bytes of
        Left err -> Left err
        Right (val, rest) -> Right (Symbol val, rest)
    0x14 -> Right (Void, bytes)
    _ -> Left "Unknown value type"

parseEnvValue :: [Char] -> Either String (EnvValue, [Char])
parseEnvValue [] = Left "No value provided"
parseEnvValue (c : bytes) 
    | fromEnum c == 0x30 = case parseInt bytes of
        Left err -> Left err
        Right (len, rest) -> case parseInstructions insts of
            Left err -> Left err
            Right func -> if length rest < len then Left "Wrong function body length" else Right (Function func, rest')
          where
            (insts, rest') = splitAt len rest
    | fromEnum c == 0x31 = case parseValue bytes of
        Left err -> Left err
        Right (val, rest) -> Right (Variable val, rest)
    | otherwise = Left "Unknown value type"

parseInstructions :: [Char] -> Either String [Insts]
parseInstructions [] = Right []
parseInstructions (c : bytes) = case fromEnum c of
    0x01 -> case parseValue bytes of
        Left err -> Left err
        Right (val, rest) -> pursueParsing (Push val) rest
    0x02 -> case parseInt bytes of
        Left err -> Left err
        Right (val, rest) -> pursueParsing (PushArg val) rest
    0x03 -> case parseString bytes of
        Left err -> Left err
        Right (val, rest) -> pursueParsing (PushEnv val) rest
    0x04 -> pursueParsing Call bytes
    0x05 -> case parseString bytes of
        Left err -> Left err
        Right (name, rest) -> case parseEnvValue rest of
            Left err -> Left err
            Right (val, rest') -> pursueParsing (DefineEnv name val) rest'
    0x06 -> case parseInt bytes of
        Left err -> Left err
        Right (val, rest) -> pursueParsing (JumpIfFalse val) rest
    0x07 -> pursueParsing Ret bytes
    _ -> Left "Unknown instruction"

pursueParsing :: Insts -> [Char] -> Either String [Insts]
pursueParsing inst rest = case parseInstructions rest of
    Left err -> Left err
    Right insts -> Right (inst : insts)

getFromBytecode :: [Char] -> Either String [Insts]
getFromBytecode ('d' : 'b' : '4' : '\n' : bytes) = parseInstructions bytes
getFromBytecode _ = Left "Exec format error"