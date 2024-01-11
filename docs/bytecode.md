# Bytecode definitions

Here are all the definitions of the bytecode:

## Instructions

- `0x01`: Push (+ Value)
- `0x02`: PushArg (+ Int)
- `0x03`: PushEnv (+ String)
- `0x04`: Call
- `0x05`: DefineEnv (+ String, Bool and Bool and EnvValue if true)
- `0x06`: EraseEnv
- `0x07`: Jump (+ Int) (+ TriBool)
- `0x08`: Ret

## Values

- `0x11` + 4 bytes: Int
- `0x12` + 1 byte: Bool
- `0x13`: String
- `0x14` + (Symbol or function name): Symbol
- `0x15`: Void

Strings: 4 bytes for length + string content

> :warning: Strings are not a value type in the language.

## Symbols

- `0x21`: Builtin + 1 byte: Operator
- `0x22`: Function Name

## Builtins

- `0x31`: Add
- `0x32`: Sub
- `0x33`: Mul
- `0x34`: Div
- `0x35`: Mod
- `0x36`: Eq
- `0x37`: Neq
- `0x38`: Less
- `0x39`: LessOrEqual
- `0x3A`: Greater
- `0x3B`: GreaterOrEqual

## DefineEnv

Both DefineEnv and DefineEnvFromStack use 1 byte to determine the type of the definition:

- `0x41`: Function (+ 4 bytes instructions length)
- `0x42`: Value

## TriBools

TriBools are a special type for the handling of conditional jumps:

- `0x51`: True
- `0x52`: False
- `0x53`: Nothing
