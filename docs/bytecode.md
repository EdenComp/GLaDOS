Here are all the definitions of the bytecode:

# Instructions

`0x01`: Push (+ Value)
`0x02`: PushArg (+ Int)
`0x03`: PushEnv (+ String)
`0x04`: Call
`0x05`: DefineEnv (+ String and EnvValue)
`0x06`: JumpIfFalse (+ Int)

# Values

`0x11` + 4 bytes: Int
`0x12` + 1 byte: Bool

Strings: 4 bytes for length + string content

> :warn: Strings are not a value type in the language.

# Symbols
`0x20`: Add
`0x21`: Sub
... and so on for the builtins, until:
`0x2A`: Function Name

# DefineEnv

This instructions uses 1 byte to determine the type of the definition:
`0x30`: Function (+ 4 bytes instructions length)
`0x31`: Value
