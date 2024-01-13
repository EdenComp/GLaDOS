module Dreamberd.Parsing.Parser (Parser, parseChar, parseAnyChar, parseAnd, parseAndWith, parseMany, parseSome, parseDreamberd, parseIf) where

import Control.Applicative (Alternative (..))
import Dreamberd.Types (AstNode (..))

newtype Parser a = Parser {parse :: (String, Int) -> Either String (a, (String, Int))}

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
            Right (result, rest) -> parse (f result) rest
            Left err -> Left err

parseChar :: Char -> Parser Char
parseChar c = Parser f
  where
    f (x : xs, index)
        | x == c = Right (c, (xs, index + 1))
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
    parseFunctionDeclaration
        <|> parseIfStatement
        <|> parseWhileLoop
        <|> parseForLoop
        <|> parseStatementExpression

parseStatementExpression :: Parser AstNode
parseStatementExpression =
    ( parseVariableDeclaration
        <|> parseReturn
        <|> parseExpression
    )
        <* parseStripped (parseChar ';')

parseExpression :: Parser AstNode
parseExpression = parseBinaryOperation <|> parseFunctionCall <|> parseUnaryOperation <|> parseAtom <|> parseEnclosed ("(", ")") parseExpression

parseBinaryOperation :: Parser AstNode
parseBinaryOperation =
    parseStripped (parseExpression <|> parseEnclosed ("(", ")") parseExpression)
        >>= \a ->
            parseStripped parseBinaryOperator
                >>= \op ->
                    parseStripped parseExpression
                        >>= \b ->
                            return (Call op [a, b])
parseUnaryOperation :: Parser AstNode
parseUnaryOperation =
    ( parseStripped parseUnaryPrefixOperator
        >>= \op ->
            parseStripped parseAtom
                >>= \operand -> return (Call op [operand])
    )
        <|> ( parseStripped parseAtom
                >>= \operand ->
                    parseStripped parseUnarySuffixOperator
                        >>= \op ->
                            return (Call op [operand])
            )

parseUnarySuffixOperator :: Parser String
parseUnarySuffixOperator =
    parseString "--"
        <|> parseString "++"

parseUnaryPrefixOperator :: Parser String
parseUnaryPrefixOperator =
    parseUnarySuffixOperator
        <|> parseString "!"
        <|> parseString "-"
        <|> parseString "+"

parseBinaryOperator :: Parser String
parseBinaryOperator =
    parseInfixFunctionIdentifierString
        <|> parseString "*="
        <|> parseString "/="
        <|> parseString "+="
        <|> parseString "-="
        <|> parseString "%="
        <|> parseString "=="
        <|> parseString "!="
        <|> parseString "<"
        <|> parseString ">"
        <|> parseString "<="
        <|> parseString ">="
        <|> parseString "%="
        <|> parseString "&&"
        <|> parseString "||"
        <|> parseString "**"
        <|> parseString "^"
        <|> parseString "="
        <|> parseString "*"
        <|> parseString "/"
        <|> parseString "+"
        <|> parseString "-"
        <|> parseString "%"

parseInfixFunctionIdentifierString :: Parser String
parseInfixFunctionIdentifierString = parseChar '`' *> parseIdentifierString <* parseChar '`'

parseAtom :: Parser AstNode
parseAtom = parseIdentifier <|> parseLiteral

parseLiteral :: Parser AstNode
parseLiteral = parseBoolean <|> parseStringLiteral <|> parseNumber

parseBoolean :: Parser AstNode
parseBoolean =
    parseString "true" <|> parseString "true" >>= \value -> return $ Boolean $ value == "true"

parseNumber :: Parser AstNode
parseNumber = Number <$> parseStripped (parseSome (parseAnyChar ['0' .. '9']) >>= \num -> return (read num :: Int))

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
            parseEnclosed ("(", ")") (parseStripped (parseOrValue parseFunctionCallArgs []))
                >>= \args -> return (Call identifier args)

parseFunctionCallArgs :: Parser [AstNode]
parseFunctionCallArgs =
    parseStripped parseExpression
        >>= \firstArg ->
            parseStripped (parseMany (parseChar ',' >> parseExpression))
                >>= \rest -> return (firstArg : rest)

parseDreamberd :: String -> Either String [AstNode]
parseDreamberd sourceCode = fst <$> parse (parseStripped (parseSome parseStatement)) (sourceCode, 0)

parseFunctionDeclaration :: Parser AstNode
parseFunctionDeclaration =
    parseStripped (parseString "fn")
        >> parseStripped parseIdentifierString
        >>= \identifier ->
            parseEnclosed ("(", ")") (parseStripped (parseOrValue parseFunctionDeclarationArgs []))
                >>= \args ->
                    parseStripped parseScope
                        >>= \body -> return (Function identifier args body)

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
        >> parseStripped parseConditionalScope
        >>= \(condition, body) ->
            parseStripped (((: []) <$> parseElifStatement) <|> parseOrValue parseElseStatement [])
                >>= \elseBody -> return (If condition body elseBody)

parseElifStatement :: Parser AstNode
parseElifStatement =
    parseStripped (parseString "elif")
        >> parseConditionalScope
        >>= \(condition, body) ->
            parseStripped (((: []) <$> parseElifStatement) <|> parseOrValue parseElseStatement [])
                >>= \nextBody -> return (If condition body nextBody)

parseElseStatement :: Parser [AstNode]
parseElseStatement =
    parseStripped (parseString "else")
        >> parseStripped parseScope

parseStripped :: Parser a -> Parser a
parseStripped p = parseManyWhiteSpaces *> p <* parseManyWhiteSpaces

parseWhileLoop :: Parser AstNode
parseWhileLoop =
    parseStripped (parseString "while")
        >> parseStripped parseConditionalScope
        >>= \(condition, body) ->
            return (Loop condition body Nothing Nothing)

parseForLoop :: Parser AstNode
parseForLoop =
    parseStripped (parseString "for")
        >> parseEnclosed ("(", ")") parseForParts
        >>= \(initNode, conditionNode, updateNode) ->
            parseStripped parseScope
                >>= \body -> return (Loop conditionNode body (Just initNode) (Just updateNode))

parseForParts :: Parser (AstNode, AstNode, AstNode)
parseForParts =
    parseStripped parseStatement
        >>= \initNode ->
            parseStripped parseStatementExpression
                >>= \conditionNode ->
                    parseStripped parseExpression
                        >>= \updateNode -> return (initNode, conditionNode, updateNode)

parseConditionalScope :: Parser (AstNode, [AstNode])
parseConditionalScope =
    parseEnclosed ("(", ")") parseExpression
        >>= \condition ->
            parseStripped parseScope
                >>= \body -> return (condition, body)

parseScope :: Parser [AstNode]
parseScope = parseEnclosed ("{", "}") (parseMany parseStatement) >>= \nodes -> return nodes

parseEnclosed :: (String, String) -> Parser a -> Parser a
parseEnclosed (open, close) p =
    parseStripped (parseString open)
        >> parseStripped p
        >>= \result -> parseStripped (parseString close) >> return result
