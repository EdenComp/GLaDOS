module Dreamberd.Parser (parseDreamberd, parse, Parser, Location (..), parseChar, parseAnyChar, parseAndWith, parseMany, parseSome, parseInteger, parseBinaryOperator) where

import Control.Applicative (Alternative (..))
import Control.Monad (void)
import Dreamberd.Types (AstNode (..), File (..))
import Options.Applicative (optional)

data Location = Location String Int Int deriving (Show, Eq)

newtype Parser a = Parser {parse :: (String, Location) -> Either String (a, (String, Location))}

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input ->
        case p input of
            Right (result, rest) -> Right (f result, rest)
            Left err -> Left err

instance Applicative Parser where
    pure x = Parser $ \input -> Right (x, input)
    (Parser p1) <*> (Parser p2) = Parser $ \input ->
        case p1 input of
            Right (f, rest) -> case p2 rest of
                Right (result, rest') -> Right (f result, rest')
                Left err -> Left err
            Left err -> Left err

instance Alternative Parser where
    empty = Parser $ \(remaining, Location fileName line col) -> Left $ "Remaining '" ++ remaining ++ "' at " ++ fileName ++ ":" ++ show line ++ ":" ++ show col
    (Parser p1) <|> (Parser p2) = Parser $ \input ->
        case p1 input of
            Right (result, rest) -> Right (result, rest)
            Left _ -> p2 input

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \input ->
        case p input of
            Right (result, rest) -> parse (f result) rest
            Left err -> Left err

instance MonadFail Parser where
    fail err = Parser $ \(_, Location fileName line col) -> Left (fileName ++ ":" ++ show line ++ ":" ++ show col ++ ": " ++ err)

parseChar :: Char -> Parser Char
parseChar c = Parser parseFunction
  where
    parseFunction (x : xs, Location fileName line col)
        | x == c = Right (x, (xs, case x of '\n' -> Location fileName (line + 1) 1; _ -> Location fileName line (col + 1)))
        | otherwise = Left $ fileName ++ ":" ++ show line ++ ":" ++ show col ++ ": Expected '" ++ [c] ++ "' but found '" ++ [x] ++ "'"
    parseFunction (_, Location fileName line col) = Left $ fileName ++ ":" ++ show line ++ ":" ++ show col ++ ": Expected '" ++ [c] ++ "' but found end of file"

parseAnythingBut :: Char -> Parser Char
parseAnythingBut c = Parser parseFunction
  where
    parseFunction (x : xs, Location fileName line col)
        | x == c = Left $ fileName ++ ":" ++ show line ++ ":" ++ show col ++ ": Expected any character but not '" ++ [c] ++ "' but found '" ++ [x] ++ "'"
        | otherwise = Right (x, (xs, case x of '\n' -> Location fileName (line + 1) 1; _ -> Location fileName line (col + 1)))
    parseFunction (_, Location fileName line col) = Left $ fileName ++ ":" ++ show line ++ ":" ++ show col ++ ": Expected any character but not '" ++ [c] ++ "' but found end of file"

parseAnyChar :: String -> Parser Char
parseAnyChar = foldl (<|>) empty . map parseChar

parseAnyString :: [String] -> Parser String
parseAnyString = foldl (<|>) empty . map parseString

parseEscapeSequence :: Parser Char
parseEscapeSequence =
    ( parseString "\\\""
        >> return '\"'
    )
        <|> ( parseString "\\\\"
                >> return '\\'
            )
        <|> ( parseString "\\n"
                >> return '\n'
            )
        <|> ( parseString "\\t"
                >> return '\t'
            )
        <|> ( parseString "\\r"
                >> return '\r'
            )
        <|> ( parseString "\\b"
                >> return '\b'
            )
        <|> ( parseString "\\f"
                >> return '\f'
            )
        <|> ( parseString "\\v"
                >> return '\v'
            )
        <|> ( parseString "\\a"
                >> return '\a'
            )
        <|> ( parseString "\\0"
                >> return '\0'
            )
        <|> ( parseString "\\e"
                >> return '\ESC'
            )

parseString :: String -> Parser String
parseString s = traverse parseChar s

parseSomeWhiteSpaces :: Parser String
parseSomeWhiteSpaces = parseSome $ parseAnyChar " \n\t"

parseManyWhiteSpaces :: Parser String
parseManyWhiteSpaces = parseMany $ parseAnyChar " \n\t"

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = f <$> p1 <*> p2

parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \input ->
    case parse (parseSome p) input of
        Right (results, rest) -> Right (results, rest)
        Left _ -> Right ([], input)

parseOrValue :: Parser a -> a -> Parser a
parseOrValue p v = Parser $ \input ->
    case parse p input of
        Right (result, rest) -> Right (result, rest)
        Left _ -> Right (v, input)

parseSome :: Parser a -> Parser [a]
parseSome p = (:) <$> p <*> parseMany p

parseStatement :: Parser AstNode
parseStatement =
    ( parseLineComments
        *> ( parseFunctionDeclaration
                <|> parseConditionalStatement "if"
                <|> parseWhileLoop
                <|> parseForLoop
                <|> (Scope <$> parseScope)
                <|> parseStatementExpression
           )
        <* parseLineComments
    )

parseLineComments :: Parser [()]
parseLineComments = parseMany parseLineComment

parseLineComment :: Parser ()
parseLineComment =
    void $
        parseManyWhiteSpaces
            *> parseString "//"
            *> parseMany (parseAnythingBut '\n')
            *> parseMany (parseAnyChar " \t")
            *> parseChar '\n'

parseStatementExpression :: Parser AstNode
parseStatementExpression =
    ( parseVariableDeclaration
        <|> parseReturn
        <|> parseImport
        <|> parseExpression
    )
        <* parseStripped (parseChar ';')

parseImport :: Parser AstNode
parseImport = Import <$> (parseStripped (parseString "import") *> parseStripped parseStringLiteral)

parseExpression :: Parser AstNode
parseExpression = parseBinaryOperation <|> parseUnaryOperation <|> parseAtom

parseBinaryOperation :: Parser AstNode
parseBinaryOperation =
    (\left operator right -> Call (Identifier operator) [left, right])
        <$> parseStripped parseAtom
        <*> parseStripped parseBinaryOperator
        <*> parseStripped parseExpression

parseUnaryOperation :: Parser AstNode
parseUnaryOperation = parseUnaryPrefix <|> parseUnarySuffix
  where
    parseUnaryPrefix = (\op operand -> Call (Identifier op) [operand]) <$> parseStripped parseUnaryPrefixOperator <*> parseStripped parseAtom
    parseUnarySuffix = (\operand op -> Call (Identifier op) [operand]) <$> parseStripped parseAtom <*> parseStripped parseUnarySuffixOperator

parseUnarySuffixOperator :: Parser String
parseUnarySuffixOperator = parseAnyString ["++", "--"]

parseUnaryPrefixOperator :: Parser String
parseUnaryPrefixOperator = parseUnarySuffixOperator <|> parseAnyString ["!", "-", "+"]

parseBinaryOperator :: Parser String
parseBinaryOperator =
    parseInfixFunctionIdentifierString
        <|> parseAnyString
            [ "*="
            , "/="
            , "+="
            , "-="
            , "%="
            , "=="
            , "!="
            , "<="
            , ">="
            , "%="
            , "&&="
            , "||="
            , "&&"
            , "||"
            , "**"
            , "<"
            , ">"
            , "^"
            , "="
            , "*"
            , "/"
            , "+"
            , "-"
            , "%"
            ]

parseInfixFunctionIdentifierString :: Parser String
parseInfixFunctionIdentifierString = parseChar '`' *> parseIdentifierString <* parseChar '`'

parseAtom :: Parser AstNode
parseAtom = parseFunctionCall <|> parseEnclosed ("(", ")") parseExpression <|> parseLiteral <|> parseLambda <|> parseIdentifier

parseLiteral :: Parser AstNode
parseLiteral = parseBoolean <|> (String <$> parseStringLiteral) <|> parseFloat <|> (Integer <$> parseInteger)

parseBoolean :: Parser AstNode
parseBoolean =
    Boolean . (== "true") <$> parseAnyString ["true", "false"]

parseInteger :: Parser Int
parseInteger = read <$> parseStripped (parseSome (parseAnyChar ['0' .. '9']))

parseFloat :: Parser AstNode
parseFloat = do
    integerPart <- parseOrValue (parseSome (parseAnyChar ['0' .. '9'])) "0"
    void $ parseChar '.'
    decimalPart <- parseSome (parseAnyChar ['0' .. '9'])
    return $ Float (read (integerPart ++ "." ++ decimalPart) :: Double)

parseStringLiteral :: Parser String
parseStringLiteral = parseChar '\"' *> parseMany (parseEscapeSequence <|> parseAnythingBut '\"') <* parseChar '\"'

parseVariableDeclaration :: Parser AstNode
parseVariableDeclaration = do
    void parseManyWhiteSpaces
    typeIdentifier <- parseTypeIdentifier
    void parseSomeWhiteSpaces
    variableIdentifier <- parseStripped parseIdentifier
    void $ parseStripped (parseChar '=')
    expression <- parseStripped parseExpression
    return $ Call (Identifier "=") [typeIdentifier, variableIdentifier, expression]

parseTypeIdentifier :: Parser AstNode
parseTypeIdentifier =
    Identifier
        <$> parseAnyString
            [ "int"
            , "float"
            , "str"
            , "bool"
            , "num"
            ]

parseIdentifierString :: Parser String
parseIdentifierString = parseAndWith (++) (parseSome $ parseAnyChar $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_']) (parseMany $ parseAnyChar $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_'] ++ ['0' .. '9'])

parseIdentifier :: Parser AstNode
parseIdentifier = Identifier <$> parseIdentifierString

parseFunctionCall :: Parser AstNode
parseFunctionCall =
    Call
        <$> parseStripped (parseEnclosed ("(", ")") parseCallee <|> parseCallee)
        <*> parseEnclosed ("(", ")") (parseOrValue parseFunctionCallArgs [])
parseCallee :: Parser AstNode
parseCallee = parseEnclosed ("(", ")") parseLambda <|> (Identifier <$> parseIdentifierString)

parseLambda :: Parser AstNode
parseLambda =
    Lambda
        <$> parseStripped (parseEnclosed ("|", "|") (parseOrValue parseFunctionDeclarationArgs []))
        <*> (void (parseStripped (parseString "=>")) *> parseStripped (parseScope <|> ((: []) <$> (Return . Just <$> parseExpression))))

parseFunctionCallArgs :: Parser [AstNode]
parseFunctionCallArgs =
    (:)
        <$> parseStripped parseExpression
        <*> parseStripped (parseMany (parseChar ',' >> parseStripped parseExpression))

parseDreamberd :: File String -> Either String [AstNode]
parseDreamberd (File fileName sourceCode) = parseDreamberd' (sourceCode, Location fileName 1 1)

parseDreamberd' :: (String, Location) -> Either String [AstNode]
parseDreamberd' ("", _) = Right []
parseDreamberd' infos = do
    (result, rest) <- parse parseStatement infos
    results <- parseDreamberd' rest
    Right (result : results)

parseFunctionDeclaration :: Parser AstNode
parseFunctionDeclaration =
    Function
        <$> (parseStripped (parseString "fn") *> parseStripped parseIdentifierString)
        <*> parseOrValue (parseEnclosed ("(", ")") (parseOrValue parseFunctionDeclarationArgs [])) []
        <*> parseStripped parseScope

parseFunctionDeclarationArgs :: Parser [String]
parseFunctionDeclarationArgs =
    (:)
        <$> parseStripped parseIdentifierString
        <*> parseStripped (parseMany (parseStripped (parseChar ',') *> parseStripped parseIdentifierString))

parseReturn :: Parser AstNode
parseReturn = Return <$> (parseStripped (parseString "return") *> parseStripped (optional parseExpression))

parseConditionalStatement :: String -> Parser AstNode
parseConditionalStatement keyword =
    parseStripped (parseString keyword)
        >> uncurry If
            <$> parseStripped parseConditionalScope
            <*> parseStripped ((: []) <$> parseConditionalStatement "elif" <|> parseOrValue parseElseStatement [])

parseElseStatement :: Parser [AstNode]
parseElseStatement =
    parseStripped (parseString "else")
        *> parseStripped (parseScope <|> ((: []) <$> parseStatement))

parseStripped :: Parser a -> Parser a
parseStripped p = parseManyWhiteSpaces *> p <* parseManyWhiteSpaces

parseWhileLoop :: Parser AstNode
parseWhileLoop =
    (\(condition, body) -> Loop condition body Nothing Nothing)
        <$> (parseStripped (parseString "while") *> parseStripped parseConditionalScope)

parseForLoop :: Parser AstNode
parseForLoop =
    (\(initNode, conditionNode, updateNode) body -> Loop conditionNode body (Just initNode) (Just updateNode))
        <$> (parseStripped (parseString "for") *> parseEnclosed ("(", ")") parseForParts)
        <*> parseStripped (parseScope <|> ((: []) <$> parseStatement))

parseForParts :: Parser (AstNode, AstNode, AstNode)
parseForParts =
    (,,)
        <$> parseStripped parseStatement
        <*> parseStripped parseStatementExpression
        <*> parseStripped parseExpression

parseConditionalScope :: Parser (AstNode, [AstNode])
parseConditionalScope =
    (,)
        <$> parseEnclosed ("(", ")") parseExpression
        <*> parseStripped (parseScope <|> ((: []) <$> parseStatement))

parseScope :: Parser [AstNode]
parseScope =
    parseEnclosed
        ("{", "}")
        ( parseLineComments
            *> ( getStatements
                    <$> parseMany parseStatement
                    <*> parseStripped (optional parseExpression)
               )
            <* parseLineComments
        )
  where
    getStatements statements Nothing = statements
    getStatements statements expression = statements ++ [Return expression]

parseEnclosed :: (String, String) -> Parser a -> Parser a
parseEnclosed (open, close) p = parseStripped (parseString open) *> parseStripped p <* parseStripped (parseString close)
