module Dreamberd.Parsing.Parser (Parser, parseChar, parseAnyChar, parseAnd, parseAndWith, parseMany, parseSome, parseDreamberd, parseIf) where

import Control.Applicative (Alternative (..))
import Debug.Trace (trace)
import Dreamberd.Types (AstNode (..))

newtype Parser a = Parser {prout :: (String, Int) -> Either String (a, (String, Int))}

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
    empty = Parser $ \(remaining, index) -> Left ("'" ++ remaining ++ "' at " ++ show index)
    (Parser p1) <|> (Parser p2) = Parser $ \input ->
        case p1 input of
            Right (result, rest) -> Right (result, rest)
            Left _ -> p2 input

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \input ->
        case p input of
            Right (result, rest) -> prout (f result) rest
            Left err -> Left err

parseChar :: Char -> Parser Char
parseChar c = Parser f
  where
    f (x : xs, index)
        | x == c = trace ("found " ++ [x] ++ " at " ++ show index ++ ", remaining: '" ++ xs ++ "'") Right (c, (xs, index + 1))
        | otherwise = Left $ "Expected '" ++ [c] ++ "' but found '" ++ [x] ++ "' at " ++ show index
    f (_, index) = Left $ "Expected '" ++ [c] ++ "' but found end of file at " ++ show index

parseAnyChar :: String -> Parser Char
parseAnyChar = foldr ((<|>) . parseChar) empty

parseString :: String -> Parser String
parseString = traverse parseChar

parseSomeWhiteSpaces :: Parser String
parseSomeWhiteSpaces = parseSome $ parseAnyChar " \n\t"

parseManyWhiteSpaces :: Parser String
parseManyWhiteSpaces = parseMany $ parseAnyChar " \n\t"

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = (,) <$> p1 <*> p2

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = f <$> p1 <*> p2

parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \input ->
    case prout (parseSome p) input of
        Right (results, rest) -> Right (results, rest)
        Left _ -> Right ([], input)

parseIf :: Show a => Parser a -> (a -> Bool) -> Parser a
parseIf p f = Parser $ \input ->
    case prout p input of
        Right (result, rest)
            | f result -> Right (result, rest)
            | otherwise -> Left (show result ++ "does not match the predicate")
        Left err -> Left err

parseOrValue :: Parser a -> a -> Parser a
parseOrValue p v = Parser $ \input ->
    case prout p input of
        Right (result, rest) -> Right (result, rest)
        Left _ -> Right (v, input)

parseSome :: Parser a -> Parser [a]
parseSome p = (:) <$> p <*> parseMany p

parseStatement :: Parser AstNode
parseStatement = (parseFunctionDeclaration <|> parseIfStatement <|> parseReturn <|> parseVariableDeclaration <|> parseExpression) <* parseStripped (parseChar ';')

parseExpression :: Parser AstNode
parseExpression = parseUnaryOperation <|> parseBinaryOperation <|> parseFunctionCall <|> parseAtom

parseBinaryOperation :: Parser AstNode
parseBinaryOperation =
    parseAtom
        >>= \a ->
            parseStripped parseBinaryOperator
                >>= \op ->
                    parseStripped parseExpression
                        >>= \b ->
                            return (Call op [a, b])
parseUnaryOperation :: Parser AstNode
parseUnaryOperation =
    ( parseStripped (parseUnaryOperator <|> parseString "!")
        >>= \op ->
            parseStripped parseAtom
                >>= \operand -> return (Call op [operand])
    )
        <|> parseStripped parseAtom
        >>= \operand ->
            parseStripped parseUnaryOperator
                >>= \op -> return (Call op [operand])

parseUnaryOperator :: Parser String
parseUnaryOperator =
    parseString "--"
        <|> parseString "++"

parseBinaryOperator :: Parser String
parseBinaryOperator =
    parseString "+"
        <|> parseString "-"
        <|> parseString "*"
        <|> parseString "/"
        <|> parseString "%"
        <|> parseString "=="
        <|> parseString "!="
        <|> parseString "<"
        <|> parseString ">"
        <|> parseString "<="
        <|> parseString ">="
        <|> parseString "="
        <|> parseString "&&"
        <|> parseString "||"
        <|> parseString "+="
        <|> parseString "-="
        <|> parseString "*="
        <|> parseString "/="
        <|> parseString "%="
        <|> parseInfixFunctionIdentifierString

parseInfixFunctionIdentifierString :: Parser String
parseInfixFunctionIdentifierString = parseChar '`' *> parseIdentifierString <* parseChar '`'

parseAtom :: Parser AstNode
parseAtom = parseIdentifier <|> parseLiteral

parseLiteral :: Parser AstNode
parseLiteral = parseBoolean <|> parseStringLiteral <|> parseNumber

parseNumber :: Parser AstNode
parseNumber = Number <$> parseStripped (parsePositiveNumber <|> parseNegativeNumber)

parsePositiveNumber :: Parser Int
parsePositiveNumber = parseSome (parseAnyChar ['0' .. '9']) >>= \num -> return $ read num

parseNegativeNumber :: Parser Int
parseNegativeNumber = (\n -> (-n)) <$> (parseStripped (parseChar '-') >> parseStripped parsePositiveNumber)

parseBoolean :: Parser AstNode
parseBoolean = parseString "true" <|> parseString "false" >>= \str -> return $ Boolean $ str == "true"

parseStringLiteral :: Parser AstNode
parseStringLiteral = parseChar '"' >> parseMany (parseAnyChar $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_'] ++ ['0' .. '9']) >>= \str -> parseChar '"' >> return (String str)

parseVariableDeclaration :: Parser AstNode
parseVariableDeclaration =
    parseManyWhiteSpaces
        >> parseIdentifier
        >>= \typeIdentifier ->
            parseSomeWhiteSpaces
                >> parseStripped parseIdentifier
                >>= \variableIdentifier ->
                    parseStripped (parseChar '=')
                        >> parseStripped parseExpression
                        >>= \expression -> return $ Call "=" [typeIdentifier, variableIdentifier, expression]

parseIdentifierString :: Parser String
parseIdentifierString = parseAndWith (++) (parseSome $ parseAnyChar $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_']) (parseMany $ parseAnyChar $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_'] ++ ['0' .. '9'])

parseIdentifier :: Parser AstNode
parseIdentifier = Identifier <$> parseIdentifierString

parseFunctionCall :: Parser AstNode
parseFunctionCall =
    parseStripped parseIdentifierString
        >>= \identifier ->
            parseStripped (parseChar '(')
                >> parseStripped (parseOrValue parseFunctionCallArgs [])
                >>= \args -> parseStripped (parseChar ')') >> return (Call identifier args)

parseFunctionCallArgs :: Parser [AstNode]
parseFunctionCallArgs =
    parseStripped parseExpression
        >>= \firstArg ->
            parseStripped (parseMany (parseChar ',' >> parseExpression))
                >>= \rest -> return (firstArg : rest)

parseDreamberd :: String -> Either String [AstNode]
parseDreamberd sourceCode = fst <$> prout (parseStripped (parseSome parseStatement)) (sourceCode, 0)

parseFunctionDeclaration :: Parser AstNode
parseFunctionDeclaration =
    parseStripped (parseString "fn")
        >> parseStripped parseIdentifierString
        >>= \identifier ->
            parseStripped (parseChar '(')
                >> parseStripped (parseOrValue parseFunctionDeclarationArgs [])
                >>= \args ->
                    parseStripped (parseChar ')')
                        >> parseStripped (parseChar '{')
                        >> parseStripped (parseSome parseStatement)
                        >>= \body ->
                            parseStripped (parseChar '}')
                                >> return (Function identifier args body)

parseFunctionDeclarationArgs :: Parser [String]
parseFunctionDeclarationArgs =
    parseStripped parseIdentifierString
        >>= \firstArg ->
            parseStripped (parseMany (parseStripped (parseChar ',') >> parseStripped parseIdentifierString))
                >>= \rest -> return (firstArg : rest)

parseReturn :: Parser AstNode
parseReturn =
    parseStripped (parseString "return")
        >> parseStripped parseExpression
        >>= \expression -> return (Return expression)

parseIfStatement :: Parser AstNode
parseIfStatement =
    parseStripped (parseString "if")
        >> parseStripped (parseChar '(')
        >> parseStripped parseExpression
        >>= \condition ->
            parseStripped (parseChar ')')
                >> parseStripped (parseChar '{')
                >> parseStripped (parseSome parseStatement)
                >>= \body ->
                    parseStripped (parseChar '}')
                        >> parseStripped (parseOrValue parseElseStatement [])
                        >>= \elseBody -> return (If condition body elseBody)

parseElseStatement :: Parser [AstNode]
parseElseStatement =
    parseStripped (parseString "else")
        >> parseStripped (parseChar '{')
        >> parseStripped (parseSome parseStatement)
        >>= \elseBody ->
            parseStripped (parseChar '}')
                >> return elseBody

parseStripped :: Parser a -> Parser a
parseStripped p = parseManyWhiteSpaces *> p <* parseManyWhiteSpaces
