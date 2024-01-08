module Lisp.Parsing (
    parse,
    Parser,
    parseChar,
    parseAnyChar,
    parseOr,
    parseAnd,
    parseAndWith,
    parseMany,
    parseSome,
    parseUInt,
    parseInt,
    parsePair,
    parseLisp,
    parseWhiteSpace,
) where

import Control.Applicative (Alternative (..))
import Control.Monad ((>=>))
import Data.Char (isDigit)
import Lisp.SExpr (SymbolicExpression (..))

newtype Parser a = Parser {parse :: String -> Maybe (a, String)}

instance Functor Parser where
    fmap f (Parser p) = Parser $ p >=> \(result, rest) -> Just (f result, rest)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    (Parser p1) <*> (Parser p2) =
        Parser $ p1 >=> \(f, rest) -> p2 rest >>= \(result, rest') -> Just (f result, rest')

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input ->
        case p1 input of
            Just (result, rest) -> Just (result, rest)
            Nothing -> p2 input

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ p >=> \(result, rest) -> parse (f result) rest

parseChar :: Char -> Parser Char
parseChar c = Parser f
  where
    f (x : xs)
        | x == c = Just (c, xs)
        | otherwise = Nothing
    f _ = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar = foldr ((<|>) . parseChar) empty

parseWhiteSpace :: Parser Char
parseWhiteSpace = parseAnyChar "\t\n "

parseOr :: Parser a -> Parser a -> Parser a
parseOr = (<|>)

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = (,) <$> p1 <*> p2

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = f <$> p1 <*> p2

parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \input ->
    case parse (parseSome p) input of
        Just (results, rest) -> Just (results, rest)
        Nothing -> Just ([], input)

parseSome :: Parser a -> Parser [a]
parseSome p =
    Parser $ parse p >=> \(result, rest) -> case parse (parseMany p) rest of
        Just (results, rest') -> Just (result : results, rest')
        Nothing -> Just ([result], rest)

parseUInt :: Parser Int
parseUInt = Parser f
  where
    f input = case span isDigit input of
        ("", _) -> Nothing
        (numStr, rest) -> Just (read numStr, rest)

parseInt :: Parser Int
parseInt = parseNeg <|> parseUInt
  where
    parseNeg = parseChar '-' *> (negate <$> parseUInt)

parsePair :: Parser a -> Parser (a, a)
parsePair p = parseChar '(' *> parseAndWith (,) p (parseChar ' ' *> p) <* parseChar ')'

parseLispNumber :: Parser SymbolicExpression
parseLispNumber = Number . toInteger <$> parseInt

parseLispSymbol :: Parser SymbolicExpression
parseLispSymbol = Symbol <$> parseAndWith (++) (parseSome (parseAnyChar (show ['a' .. 'z'] ++ show ['A' .. 'Z'] ++ "+-*/<>=#?"))) (parseMany (parseAnyChar (show ['a' .. 'z'] ++ show ['A' .. 'Z'] ++ "+-*/<>=#?" ++ show ['0' .. '9'])))

parseLispList :: Parser SymbolicExpression
parseLispList = parseChar '(' *> (List <$> parseSome parseLispExpr) <* parseChar ')'

parseLispExpr :: Parser SymbolicExpression
parseLispExpr = parseMany parseWhiteSpace *> (parseLispNumber <|> parseLispSymbol <|> parseLispList) <* parseMany parseWhiteSpace

parseLisp :: String -> Maybe [SymbolicExpression]
parseLisp input = fst <$> parse (parseSome parseLispExpr) input
