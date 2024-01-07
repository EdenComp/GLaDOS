module Args (
    Command (..),
    commandsParser,
) where

import Options.Applicative

data Command
    = Compile String String
    | Execute String
    | Lisp (Maybe String)
    | Version
    deriving (Show)

commandsParser :: ParserInfo Command
commandsParser = (info (helper <*> hsubparser (compileCommand <> executeCommand <> lispCommand <> versionCommand)) (fullDesc <> progDesc "GLaDOS - Compile & Execute Dreamberd, Interpret LISP" <> header "GLaDOS")){infoFailureCode = 84}

compileCommand :: Mod CommandFields Command
compileCommand = command "compile" (info compileOptions (progDesc "Compile DreamBerd source code"))

compileOptions :: Parser Command
compileOptions =
    Compile
        <$> strArgument (metavar "FILE" <> help "File to compile")
        <*> strOption (short 'o' <> long "output" <> value "a.out" <> metavar "OUTPUT" <> help "Output file")

executeCommand :: Mod CommandFields Command
executeCommand = command "execute" (info executeOptions (progDesc "Execute compiled Dreamberd code"))

executeOptions :: Parser Command
executeOptions = Execute <$> strArgument (metavar "FILE" <> help "File to execute")

lispCommand :: Mod CommandFields Command
lispCommand = command "lisp" (info lispOptions (progDesc "Interpret LISP code"))

lispOptions :: Parser Command
lispOptions = Lisp <$> optional (strArgument (metavar "FILE" <> help "File to interpret"))

versionCommand :: Mod CommandFields Command
versionCommand = command "version" (info (pure Version) (progDesc "Get program version"))
