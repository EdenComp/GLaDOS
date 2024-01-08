module Args (
    Command (..),
    commandsParser,
) where

import Options.Applicative

data Command
    = Compile String String Bool Bool
    | Execute String
    | Run String
    | Lisp (Maybe String)
    | Version
    deriving (Show)

commandsParser :: ParserInfo Command
commandsParser = (info (helper <*> hsubparser (compileCommand <> executeCommand <> runCommand <> lispCommand <> versionCommand)) (fullDesc <> progDesc "GLaDOS - Compile & Execute Dreamberd, Interpret LISP" <> header "GLaDOS")){infoFailureCode = 84}

compileCommand :: Mod CommandFields Command
compileCommand = command "compile" (info compileOptions (progDesc "Compile DreamBerd source code"))

compileOptions :: Parser Command
compileOptions =
    Compile
        <$> strArgument (metavar "FILE" <> help "File to compile")
        <*> strOption (short 'o' <> long "output" <> value "a.out" <> metavar "OUTPUT" <> help "Output file")
        <*> switch (short 'a' <> long "ast" <> help "Get the program Abstract Syntax Tree")
        <*> switch (short 'i' <> long "vm-insts" <> help "Get the program compiled instructions")

executeCommand :: Mod CommandFields Command
executeCommand = command "execute" (info executeOptions (progDesc "Execute compiled Dreamberd code"))

executeOptions :: Parser Command
executeOptions = Execute <$> strArgument (metavar "FILE" <> help "File to execute")

runCommand :: Mod CommandFields Command
runCommand = command "run" (info runOptions (progDesc "Run Dreamberd code"))

runOptions :: Parser Command
runOptions = Run <$> strArgument (metavar "FILE" <> help "File to run")

lispCommand :: Mod CommandFields Command
lispCommand = command "lisp" (info lispOptions (progDesc "Interpret LISP code"))

lispOptions :: Parser Command
lispOptions = Lisp <$> optional (strArgument (metavar "FILE" <> help "File to interpret"))

versionCommand :: Mod CommandFields Command
versionCommand = command "version" (info (pure Version) (progDesc "Get program version"))
