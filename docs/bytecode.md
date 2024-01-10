# Bytecode definitions

Here are all the definitions of the bytecode:

## Instructions

- `0x01`: Push (+ Value)
- `0x02`: PushArg (+ Int)
- `0x03`: PushEnv (+ String)
- `0x04`: Call
- `0x05`: DefineEnv (+ String and EnvValue)
- `0x06`: DefineEnvFromStack (+ String)
- `0x07`: Jump (+ Int)
- `0x08`: JumpIfFalse (+ Int)
- `0x09`: Ret

## Values

`0x11` + 4 bytes: Int
`0x12` + 1 byte: Bool
`0x13`: String
`0x14` + (Symbol or function name): Symbol
`0x15`: Void

Strings: 4 bytes for length + string content

> :warning: Strings are not a value type in the language.

## Symbols
`0x21`: Add
`0x22`: Sub
... and so on for the builtins, until:
`0x2B`: Function Name

## DefineEnv

This instructions uses 1 byte to determine the type of the definition:
`0x31`: Function (+ 4 bytes instructions length)
`0x32`: Value
