<a name="v1.0.0"></a>
# [v1.0.0 🌈](https://github.com/EdenComp/GLaDOS/releases/tag/v1.0.0) - 14 Jan 2024

## Changes

Last project release (after 2 LoL games)

## 🚀 Features

- Lambdas [@EdenComp](https://github.com/EdenComp) ([#102](https://github.com/EdenComp/GLaDOS/issues/102))
- BNF updated with latest code changes [@RezaRahemtola](https://github.com/RezaRahemtola) ([#117](https://github.com/EdenComp/GLaDOS/issues/117))
- feat(parsing): variables assignations types can now only be certain values [@Croos3r](https://github.com/Croos3r) ([#114](https://github.com/EdenComp/GLaDOS/issues/114))
- fix: unit tests on builtin operations [@EdenComp](https://github.com/EdenComp) ([#115](https://github.com/EdenComp/GLaDOS/issues/115))
- feat: unit tests on new vm operators [@EdenComp](https://github.com/EdenComp) ([#112](https://github.com/EdenComp/GLaDOS/issues/112))
- feat: Preprocessing (2) - Optimizations [@EdenComp](https://github.com/EdenComp) ([#110](https://github.com/EdenComp/GLaDOS/issues/110))
- Add 3 functional tests on complex programs [@Tomi-Tom](https://github.com/Tomi-Tom) ([#104](https://github.com/EdenComp/GLaDOS/issues/104))
- feat: IO improvements (error builtin + flush on input) [@EdenComp](https://github.com/EdenComp) ([#108](https://github.com/EdenComp/GLaDOS/issues/108))
- feat: operators between integers and floats [@EdenComp](https://github.com/EdenComp) ([#109](https://github.com/EdenComp/GLaDOS/issues/109))
- add unit tests for parsing [@pablo0675](https://github.com/pablo0675) ([#107](https://github.com/EdenComp/GLaDOS/issues/107))
- Make functional tests on everything [@Tomi-Tom](https://github.com/Tomi-Tom) ([#101](https://github.com/EdenComp/GLaDOS/issues/101))
- fix: Function arguments overrides [@EdenComp](https://github.com/EdenComp) ([#100](https://github.com/EdenComp/GLaDOS/issues/100))
- Implementation of singular scopes  [@Croos3r](https://github.com/Croos3r) ([#98](https://github.com/EdenComp/GLaDOS/issues/98))
- feat(parser): now handle \\ " \\t \\n \\r \\v \\f \\a \\b \\e \\0 [@Tomi-Tom](https://github.com/Tomi-Tom) ([#99](https://github.com/EdenComp/GLaDOS/issues/99))
- add last unit tests [@pablo0675](https://github.com/pablo0675) ([#116](https://github.com/EdenComp/GLaDOS/issues/116))
- Add Unit test for Dreamberd Compile [@pablo0675](https://github.com/pablo0675) ([#111](https://github.com/EdenComp/GLaDOS/issues/111))
- Update changelog for "v0.9.0" [@github-actions](https://github.com/github-actions) ([#97](https://github.com/EdenComp/GLaDOS/issues/97))

## 📄 Documentation

- BNF updated with latest code changes [@RezaRahemtola](https://github.com/RezaRahemtola) ([#117](https://github.com/EdenComp/GLaDOS/issues/117))
- Project documentation [@RezaRahemtola](https://github.com/RezaRahemtola) ([#113](https://github.com/EdenComp/GLaDOS/issues/113))


[Changes][v1.0.0]


<a name="v0.9.0"></a>
# [v0.9.0 🌈](https://github.com/EdenComp/GLaDOS/releases/tag/v0.9.0) - 14 Jan 2024

## Changes

Multiple parsing and pre processing improvements

## 🚀 Features

- feat(preprocess): Function hoisting [@EdenComp](https://github.com/EdenComp) ([#94](https://github.com/EdenComp/GLaDOS/issues/94))
- Parsing: Returning without returning [@Croos3r](https://github.com/Croos3r) ([#95](https://github.com/EdenComp/GLaDOS/issues/95))
- Compilation: generalization of the unary operators + and - to all expression [@Croos3r](https://github.com/Croos3r) ([#93](https://github.com/EdenComp/GLaDOS/issues/93))
- Now taking argument as variables (making them mutable) [@EdenComp](https://github.com/EdenComp) ([#92](https://github.com/EdenComp/GLaDOS/issues/92))
- input function to read stdin [@EdenComp](https://github.com/EdenComp) ([#91](https://github.com/EdenComp/GLaDOS/issues/91))
- We can now return void/nothing instead of an expression [@Croos3r](https://github.com/Croos3r) ([#90](https://github.com/EdenComp/GLaDOS/issues/90))
- Preprocessing (1): Imports [@EdenComp](https://github.com/EdenComp) ([#74](https://github.com/EdenComp/GLaDOS/issues/74))
- Parsing: syntactic sugar for scopes with only one statement [@Croos3r](https://github.com/Croos3r) ([#88](https://github.com/EdenComp/GLaDOS/issues/88))
- Parsing: Implementation of float values [@Croos3r](https://github.com/Croos3r) ([#85](https://github.com/EdenComp/GLaDOS/issues/85))
- Parsing: handling of comments [@Croos3r](https://github.com/Croos3r) ([#83](https://github.com/EdenComp/GLaDOS/issues/83))
- Removed print of the main scope return value [@EdenComp](https://github.com/EdenComp) ([#81](https://github.com/EdenComp/GLaDOS/issues/81))
- Scopes index for environment [@EdenComp](https://github.com/EdenComp) ([#79](https://github.com/EdenComp/GLaDOS/issues/79))

## 🧰 Maintenance

- fix: compilation and unit tests launch [@EdenComp](https://github.com/EdenComp) ([#87](https://github.com/EdenComp/GLaDOS/issues/87))
- build(deps): bump tj-actions/changed-files from 39 to 41 in /.github/workflows [@dependabot](https://github.com/dependabot) ([#96](https://github.com/EdenComp/GLaDOS/issues/96))
- refacto parsing unit test  [@pablo0675](https://github.com/pablo0675) ([#82](https://github.com/EdenComp/GLaDOS/issues/82))
- Update changelog for "v0.8.0" [@github-actions](https://github.com/github-actions) ([#80](https://github.com/EdenComp/GLaDOS/issues/80))

[Changes][v0.9.0]


<a name="v0.8.0"></a>
# [v0.8.0 🌈](https://github.com/EdenComp/GLaDOS/releases/tag/v0.8.0) - 13 Jan 2024

## Changes

Basic BNF, functional tests, parsing refactoring and various improvements

## 🚀 Features

- Unit tests on new VM and bytecode features [@EdenComp](https://github.com/EdenComp) ([#77](https://github.com/EdenComp/GLaDOS/issues/77))
- Full BNF with documentation integration [@RezaRahemtola](https://github.com/RezaRahemtola) ([#78](https://github.com/EdenComp/GLaDOS/issues/78))
- Update: make more Functional tests [@Tomi-Tom](https://github.com/Tomi-Tom) ([#73](https://github.com/EdenComp/GLaDOS/issues/73))
- Float handling in VM and Bytecode [@EdenComp](https://github.com/EdenComp) ([#75](https://github.com/EdenComp/GLaDOS/issues/75))
- Setup functional tests for compile + exec [@Tomi-Tom](https://github.com/Tomi-Tom) ([#72](https://github.com/EdenComp/GLaDOS/issues/72))
- Refactor DefineEnv VM instructions [@EdenComp](https://github.com/EdenComp) ([#70](https://github.com/EdenComp/GLaDOS/issues/70))
- Compilation: Builtin modulo operator [@Croos3r](https://github.com/Croos3r) ([#71](https://github.com/EdenComp/GLaDOS/issues/71))

## 🐛 Bug Fixes

- Parsing: refactoring of the mess [@Croos3r](https://github.com/Croos3r) ([#76](https://github.com/EdenComp/GLaDOS/issues/76))
- PushEnv now working for print [@EdenComp](https://github.com/EdenComp) ([#67](https://github.com/EdenComp/GLaDOS/issues/67))
- Fix CI: Timeout on release binary compilation [@RezaRahemtola](https://github.com/RezaRahemtola) ([#69](https://github.com/EdenComp/GLaDOS/issues/69))

## 📄 Documentation

- Full BNF with documentation integration [@RezaRahemtola](https://github.com/RezaRahemtola) ([#78](https://github.com/EdenComp/GLaDOS/issues/78))
- Update changelog for "v0.7.0" [@github-actions](https://github.com/github-actions) ([#68](https://github.com/EdenComp/GLaDOS/issues/68))

[Changes][v0.8.0]


<a name="v0.7.0"></a>
# [v0.7.0 🌈](https://github.com/EdenComp/GLaDOS/releases/tag/v0.7.0) - 11 Jan 2024

## Changes

Operators in parsing, VM instructions improvements and compilation

## 🚀 Features

- Compilation [@Croos3r](https://github.com/Croos3r) ([#59](https://github.com/EdenComp/GLaDOS/issues/59))
- Unit tests : Encode.hs / Types.hs [@Tomi-Tom](https://github.com/Tomi-Tom) ([#56](https://github.com/EdenComp/GLaDOS/issues/56))
- Modulo operator in VM and builtin/jump refactor [@EdenComp](https://github.com/EdenComp) ([#65](https://github.com/EdenComp/GLaDOS/issues/65))
- Jump VM instruction [@EdenComp](https://github.com/EdenComp) ([#64](https://github.com/EdenComp/GLaDOS/issues/64))
- JumpIfFalse for negative numbers [@EdenComp](https://github.com/EdenComp) ([#61](https://github.com/EdenComp/GLaDOS/issues/61))
- Parsing: Working while loop and operators as values [@RezaRahemtola](https://github.com/RezaRahemtola) ([#57](https://github.com/EdenComp/GLaDOS/issues/57))

## 🐛 Bug Fixes

- Fix parsing Parenthesis handling in function calls [@RezaRahemtola](https://github.com/RezaRahemtola) ([#66](https://github.com/EdenComp/GLaDOS/issues/66))
- Fix parsing function calls for exact match [@RezaRahemtola](https://github.com/RezaRahemtola) ([#63](https://github.com/EdenComp/GLaDOS/issues/63))

## 📄 Documentation

- Update changelog for "v0.6.0" [@github-actions](https://github.com/github-actions) ([#58](https://github.com/EdenComp/GLaDOS/issues/58))


[Changes][v0.7.0]


<a name="v0.6.0"></a>
# [v0.6.0 🌈](https://github.com/EdenComp/GLaDOS/releases/tag/v0.6.0) - 10 Jan 2024

## Changes

Unit tests improvements (parsing and VM), parsing of conditions, operators and scopres

## 🚀 Features

- Add parse conditions operators [@pablo0675](https://github.com/pablo0675) ([#37](https://github.com/EdenComp/GLaDOS/issues/37))
- Parsing: Assign and reassign variables [@RezaRahemtola](https://github.com/RezaRahemtola) ([#52](https://github.com/EdenComp/GLaDOS/issues/52))
- VM execution unit tests [@EdenComp](https://github.com/EdenComp) ([#53](https://github.com/EdenComp/GLaDOS/issues/53))
- Language improvements: case of replaced with >>= [@EdenComp](https://github.com/EdenComp) ([#51](https://github.com/EdenComp/GLaDOS/issues/51))
- Define from stack VM instruction [@EdenComp](https://github.com/EdenComp) ([#49](https://github.com/EdenComp/GLaDOS/issues/49))

## 🐛 Bug Fixes

- Fix parsing variable value type and spaces around equal sign [@RezaRahemtola](https://github.com/RezaRahemtola) ([#55](https://github.com/EdenComp/GLaDOS/issues/55))
- Parsing: If/elif/else parsing for condition [@RezaRahemtola](https://github.com/RezaRahemtola) ([#54](https://github.com/EdenComp/GLaDOS/issues/54))
- Parsing: Nested scopes handled correctly [@RezaRahemtola](https://github.com/RezaRahemtola) ([#50](https://github.com/EdenComp/GLaDOS/issues/50))
- Function parsing: Fix parenthesis in params [@RezaRahemtola](https://github.com/RezaRahemtola) ([#48](https://github.com/EdenComp/GLaDOS/issues/48))
- Parsing: Scopes and functions improvements [@RezaRahemtola](https://github.com/RezaRahemtola) ([#44](https://github.com/EdenComp/GLaDOS/issues/44))

## 📄 Documentation

- Update changelog for "v0.5.0" [@github-actions](https://github.com/github-actions) ([#47](https://github.com/EdenComp/GLaDOS/issues/47))


[Changes][v0.6.0]


<a name="v0.5.0"></a>
# [v0.5.0 🌈](https://github.com/EdenComp/GLaDOS/releases/tag/v0.5.0) - 08 Jan 2024

## Changes

VM addition, parsing improvements and code hierarchy split

## 🚀 Features

- VM print builtin [@EdenComp](https://github.com/EdenComp) ([#46](https://github.com/EdenComp/GLaDOS/issues/46))
- Compiler options to print AST or Vm instructions [@EdenComp](https://github.com/EdenComp) ([#45](https://github.com/EdenComp/GLaDOS/issues/45))
- Small improvements on program flow [@EdenComp](https://github.com/EdenComp) ([#43](https://github.com/EdenComp/GLaDOS/issues/43))
- Parsing: While loop [@RezaRahemtola](https://github.com/RezaRahemtola) ([#42](https://github.com/EdenComp/GLaDOS/issues/42))
- Arguments handling revamp [@EdenComp](https://github.com/EdenComp) ([#41](https://github.com/EdenComp/GLaDOS/issues/41))
- Strings in Bytecode and VM [@EdenComp](https://github.com/EdenComp) ([#39](https://github.com/EdenComp/GLaDOS/issues/39))
- feat: simplified IO on files and added the whole vm flow [@EdenComp](https://github.com/EdenComp) ([#38](https://github.com/EdenComp/GLaDOS/issues/38))
- feat: Bytecode encoding and decoding [@EdenComp](https://github.com/EdenComp) ([#36](https://github.com/EdenComp/GLaDOS/issues/36))
- Parsing: If, elif and else [@RezaRahemtola](https://github.com/RezaRahemtola) ([#35](https://github.com/EdenComp/GLaDOS/issues/35))
- VM Bootstrap [@EdenComp](https://github.com/EdenComp) ([#30](https://github.com/EdenComp/GLaDOS/issues/30))

## 🧰 Maintenance

- Hierarchy: Lisp and Dreamberd separation \& basic Dreamberd parsing tests [@RezaRahemtola](https://github.com/RezaRahemtola) ([#34](https://github.com/EdenComp/GLaDOS/issues/34))


[Changes][v0.5.0]


<a name="v0.4.0"></a>
# [v0.4.0 🌈](https://github.com/EdenComp/GLaDOS/releases/tag/v0.4.0) - 05 Jan 2024

## Changes

Basics of AST content generation

## 🚀 Features

- Variables parsing: Fix useless dropped char \& adding strings, booleans, functions, function calls, return and identifiers [@RezaRahemtola](https://github.com/RezaRahemtola) ([#32](https://github.com/EdenComp/GLaDOS/issues/32))
- Parsing: new AST types \& basic variables [@RezaRahemtola](https://github.com/RezaRahemtola) [@pablo0675](https://github.com/pablo0675)  ([#29](https://github.com/EdenComp/GLaDOS/issues/29))
- Add flags -h -v -vm -c -l -lr [@Tomi-Tom](https://github.com/Tomi-Tom) ([#28](https://github.com/EdenComp/GLaDOS/issues/28))

## 🐛 Bug Fixes

- Variables parsing: Fix useless dropped char \& adding strings, booleans, functions, function calls, return and identifiers [@RezaRahemtola](https://github.com/RezaRahemtola) ([#32](https://github.com/EdenComp/GLaDOS/issues/32))

## 📄 Documentation

- Update changelog for "v0.3.0" [@github-actions](https://github.com/github-actions) ([#27](https://github.com/EdenComp/GLaDOS/issues/27))


[Changes][v0.4.0]


<a name="v0.3.0"></a>
# [v0.3.0 🌈](https://github.com/EdenComp/GLaDOS/releases/tag/v0.3.0) - 19 Dec 2023

## Changes

Functional tests, small bug fixes and last mandatory operators

## 🚀 Features

- New functional test for [@Tomi-Tom](https://github.com/Tomi-Tom) [@RezaRahemtola](https://github.com/RezaRahemtola) ([#25](https://github.com/EdenComp/GLaDOS/issues/25))
- Basic functional tests [@RezaRahemtola](https://github.com/RezaRahemtola) ([#24](https://github.com/EdenComp/GLaDOS/issues/24))
- Comparison operators [@EdenComp](https://github.com/EdenComp) ([#23](https://github.com/EdenComp/GLaDOS/issues/23))
- Setup functional tests [@RezaRahemtola](https://github.com/RezaRahemtola) ([#16](https://github.com/EdenComp/GLaDOS/issues/16))
- If statements, true and false in parsing, and tests on ifs [@EdenComp](https://github.com/EdenComp) ([#19](https://github.com/EdenComp/GLaDOS/issues/19))

## 🐛 Bug Fixes

- Remove all text in TTY [@Tomi-Tom](https://github.com/Tomi-Tom) ([#20](https://github.com/EdenComp/GLaDOS/issues/20))
- Double function call bug [@Croos3r](https://github.com/Croos3r) ([#26](https://github.com/EdenComp/GLaDOS/issues/26))

## 📄 Documentation

- Docs: Sidebar with changelog [@RezaRahemtola](https://github.com/RezaRahemtola) ([#22](https://github.com/EdenComp/GLaDOS/issues/22))
- Update changelog for "v0.2.0" [@github-actions](https://github.com/github-actions) ([#21](https://github.com/EdenComp/GLaDOS/issues/21))


[Changes][v0.3.0]


<a name="v0.2.0"></a>
# [v0.2.0 🌈](https://github.com/EdenComp/GLaDOS/releases/tag/v0.2.0) - 18 Dec 2023

## Changes

Unit tests, SonarQube, documentation and lambda functions

## 🚀 Features
- Unit tests on evaluation [@EdenComp](https://github.com/EdenComp) ([#8](https://github.com/EdenComp/GLaDOS/issues/8))
- Interpreter pipeline [@Croos3r](https://github.com/Croos3r) ([#18](https://github.com/EdenComp/GLaDOS/issues/18))
- :warning: Addition of PARSING (lezgo) and some fixes on the evaluation [@Croos3r](https://github.com/Croos3r) ([#17](https://github.com/EdenComp/GLaDOS/issues/17))
- Build and documentation changelog on release CI [@RezaRahemtola](https://github.com/RezaRahemtola) ([#15](https://github.com/EdenComp/GLaDOS/issues/15))
- HLint \& Fourmolu CI [@RezaRahemtola](https://github.com/RezaRahemtola) ([#9](https://github.com/EdenComp/GLaDOS/issues/9))
- Lambda, functions and eval code restructuration [@Croos3r](https://github.com/Croos3r) ([#10](https://github.com/EdenComp/GLaDOS/issues/10))
- Setup SonarQube [@RezaRahemtola](https://github.com/RezaRahemtola) ([#14](https://github.com/EdenComp/GLaDOS/issues/14))


[Changes][v0.2.0]


<a name="v0.1.0"></a>
# [v0.1.0 🌈](https://github.com/EdenComp/GLaDOS/releases/tag/v0.1.0) - 13 Dec 2023

## Changes

Project setup with CI, evaluation of SExpr and documentation

## 🚀 Features

- Documentation linting checks [@RezaRahemtola](https://github.com/RezaRahemtola) ([#13](https://github.com/EdenComp/GLaDOS/issues/13))
- Tombariteau peter/gla 20 input reading [@Tomi-Tom](https://github.com/Tomi-Tom) ([#7](https://github.com/EdenComp/GLaDOS/issues/7))
- Removal of INTEGER and FLOAT to let place for NUMBER [@Croos3r](https://github.com/Croos3r) ([#6](https://github.com/EdenComp/GLaDOS/issues/6))
- feat: Evaluation without tests [@EdenComp](https://github.com/EdenComp) ([#5](https://github.com/EdenComp/GLaDOS/issues/5))
- feat: Haskell Compilation + Tests CI [@EdenComp](https://github.com/EdenComp) ([#4](https://github.com/EdenComp/GLaDOS/issues/4))
- feat: Created a basic Stack project [@EdenComp](https://github.com/EdenComp) ([#2](https://github.com/EdenComp/GLaDOS/issues/2))
- Release drafter setup [@RezaRahemtola](https://github.com/RezaRahemtola) ([#3](https://github.com/EdenComp/GLaDOS/issues/3))
- feat: mirror workflow [@EdenComp](https://github.com/EdenComp) ([#1](https://github.com/EdenComp/GLaDOS/issues/1))

## 🐛 Bug Fixes

- hotfix(docs): Wrong repository username [@RezaRahemtola](https://github.com/RezaRahemtola) ([#12](https://github.com/EdenComp/GLaDOS/issues/12))

## 📄 Documentation

- Basic documentation setup [@RezaRahemtola](https://github.com/RezaRahemtola) ([#11](https://github.com/EdenComp/GLaDOS/issues/11))


[Changes][v0.1.0]


[v1.0.0]: https://github.com/EdenComp/GLaDOS/compare/v0.9.0...v1.0.0
[v0.9.0]: https://github.com/EdenComp/GLaDOS/compare/v0.8.0...v0.9.0
[v0.8.0]: https://github.com/EdenComp/GLaDOS/compare/v0.7.0...v0.8.0
[v0.7.0]: https://github.com/EdenComp/GLaDOS/compare/v0.6.0...v0.7.0
[v0.6.0]: https://github.com/EdenComp/GLaDOS/compare/v0.5.0...v0.6.0
[v0.5.0]: https://github.com/EdenComp/GLaDOS/compare/v0.4.0...v0.5.0
[v0.4.0]: https://github.com/EdenComp/GLaDOS/compare/v0.3.0...v0.4.0
[v0.3.0]: https://github.com/EdenComp/GLaDOS/compare/v0.2.0...v0.3.0
[v0.2.0]: https://github.com/EdenComp/GLaDOS/compare/v0.1.0...v0.2.0
[v0.1.0]: https://github.com/EdenComp/GLaDOS/tree/v0.1.0

<!-- Generated by https://github.com/rhysd/changelog-from-release v3.7.1 -->
