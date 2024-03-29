# Bytecode definitions

Here are all the definitions of the bytecode:

## Instructions

- `0x01`: Push (+ Value)
- `0x02`: PushArg (+ Int)
- `0x03`: PushEnv (+ String)
- `0x04`: Call
- `0x05`: DefineEnv (+ String, Bool and (True and EnvValue)/(False))
- `0x06`: EraseEnv
- `0x07`: Jump (+ Int) (+ TriBool)
- `0x08`: Ret

## Values

- `0x11` + 8 bytes: Int
- `0x12` + Integer + Int: Float
- `0x13` + 1 byte: Bool
- `0x14`: String
- `0x15` + (Symbol or function name): Symbol
- `0x16` + 8 bytes args length + 8 bytes instructions length: Lambda
- `0x17`: Void

Strings: 8 bytes for length + string content
Integer: 8 bytes for length + integral number as bits

## Symbols

- `0x21`: Operator + 1 byte
- `0x22`: Builtin + 1 byte for the builtin (list below)

The language has 3 builtins to interact with the standard input / output / error:

- `0x25`: Input
- `0x26`: Print
- `0x27`: Error

## Operators

- `0x31`: Add
- `0x32`: Sub
- `0x33`: Mul
- `0x34`: Div
- `0x35`: Mod
- `0x36`: Pow
- `0x37`: Eq
- `0x38`: Neq
- `0x39`: Less
- `0x3A`: LessOrEqual
- `0x3B`: Greater
- `0x3C`: GreaterOrEqual
- `0x3D`: And
- `0x3E`: Or
- `0x3F`: Xor

## DefineEnv

There is 3 types of DefineEnv:

- `0x41`: Define (does not allow redefinitions)
- `0x42`: Redefine (does not allow definitions)
- `0x43`: Override (redefines if necessary, otherwise defines)

## TriBools

TriBools are a special type for the handling of conditional jumps:

- `0x51`: True
- `0x52`: False
- `0x53`: Nothing
