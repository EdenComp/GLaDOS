# Syntax

Let's go through the syntax of Dreamberd.

If you try it, you'll that it is quite similar to JavaScript, with some parts borrowed from
Python, C...

To understand the whole syntax, here's the BNF of the language:
```text
<syntax> ::= <empty> | <optional_whitespace> <statement> <optional_whitespace> <syntax>

<statement> ::= <var_assignment> | <var_reassignment> | <if_statement> | <while_statement> | <function_definition> | <function_call>
<identifier> ::= <identifier_char> | <identifier_char> <identifier>
<function_definition> ::= "fn" <mandatory_whitespace> <identifier> <optional_whitespace> "(" <optional_whitespace> <comma_separated_identifier_list> <optional_whitespace> ")" <optional_whitespace> <scoped_code>
<while_statement> ::= "while" <optional_whitespace> <condition> <optional_whitespace> <scoped_code>
<if_statement> ::= "if" <optional_whitespace> <condition> <optional_whitespace> <scoped_code> <optional_whitespace> <elif_statement> <else_statement>
<elif_statement> ::= <empty> | "elif" <optional_whitespace> <condition> <optional_whitespace> <scoped_code> <optional_whitespace> <elif_statement>
<else_statement> ::= <empty> | "else" <optional_whitespace> <scoped_code>
<function_call> ::= <identifier> <optional_whitespace> "(" <optional_whitespace> <function_call_params_list> <optional_whitespace> ")" <optional_whitespace> ";"
<var_assignment> ::= <var_type> <mandatory_whitespace> <identifier> <optional_whitespace> "=" <optional_whitespace> <value> <optional_whitespace> ";"
<var_reassignment> ::= <identifier> <optional_whitespace> <reassignment_operator> <optional_whitespace> <value> <optional_whitespace> ";"


<float> ::= <int> "." <int>
<int> ::= <digit> | <digit> <int>
<bool> ::= "true" | "false"
<string> ::= "\"" <ascii_char> "\""
<var_type> ::= "str" | "int" | "float" | "bool"


<function_call_params_list> ::= <value> | <value> <optional_whitespace> "," <optional_whitespace> <function_call_params_list> | <empty>
<comma_separated_identifier_list> ::= <identifier> | <identifier> <optional_whitespace> "," <optional_whitespace> <comma_separated_identifier_list> | <empty>

<value> ::= (<expression> | <infix>)
<expression> ::= <identifier> | <int> | <float> | <bool> | <string> | <function_call>
<infix> ::= <expression> <optional_whitespace> <infix_operator> <optional_whitespace> <expression>

<condition> ::= "(" <optional_whitespace> <value> <optional_whitespace> ")"
<scoped_code> ::= "{" <syntax> "}"

<infix_operator> ::= "+" | "-" | "*" | "/" | "%" | "==" | "=" | "<=" | "<" | ">=" | ">" | "!="
<reassignment_operator> ::= "=" | "+=" | "-=" | "*=" | "/=" | "%="

<ascii_char> ::= <letter> | <digit> | <complex_char>
<identifier_char> ::= <letter> | <digit> | "-" | "_"
<letter> ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<complex_char> ::= "|" | " " | "'" | "!" | "#" | "$" | "%" | "&" | "(" | ")" | "*" | "+" | "," | "-" | "." | "/" | ":" | ";" | ">" | "=" | "<" | "?" | "@" | "[" | "\\" | "]" | "^" | "_" | "`" | "{" | "}" | "~"

<mandatory_whitespace> ::= <whitespace> | <whitespace> <mandatory_whitespace>
<optional_whitespace> ::= <empty> | <whitespace> <optional_whitespace>
<whitespace> ::= " " | "\t" | "\n" | "\r"

<empty> ::= ""
```