{-# LANGUAGE ViewPatterns #-}

module NewParsing (parseDreamberd) where

import Data.Char (isDigit, isSpace)
import Data.List (isInfixOf, isPrefixOf, stripPrefix)
import NewTypes (AstNode (Boolean, Call, Function, Number, Operator, String))
import Text.Regex.Posix ((=~))

isNotSemiColon :: Char -> Bool
isNotSemiColon = (/= ';')

extractQuotedStringAndRest :: String -> (String, String)
extractQuotedStringAndRest str = (head matches, rest)
  where
    (before, match, _, matches) = str =~ "(\"[^\"]*\")" :: (String, String, String, [String])
    rest = drop (length before + length match) str

extractFunctionBodyAndRest :: String -> Int -> String -> (String, String)
extractFunctionBodyAndRest [] _ body = (body, [])
extractFunctionBodyAndRest (x : xs) openBraces body
    | x == '{' = extractFunctionBodyAndRest xs (openBraces + 1) (body ++ [x])
    | x == '}' =
        if openBraces - 1 == 0
            then (body, dropWhile isSpace xs)
            else extractFunctionBodyAndRest xs (openBraces - 1) (body ++ [x])
    | otherwise = extractFunctionBodyAndRest xs openBraces (body ++ [x])

extractFunctionParts :: String -> (String, [String], String, String)
extractFunctionParts str = (name, params, body, restOfCode)
  where
    (beforeBrace, afterBrace) = break (== '{') str
    (nameAndParams, _) = break (== ')') beforeBrace
    (name, parameters) = break (== '(') (dropWhile isSpace nameAndParams)
    params = words $ map (\c -> if c == ',' then ' ' else c) $ tail parameters
    (body, restOfCode) = extractFunctionBodyAndRest (drop 1 afterBrace) 1 []

removeSemi :: String -> String
removeSemi xs = [x | x <- xs, x `notElem` ";"]

parseBool :: String -> Either String AstNode
parseBool input
    | "true" `isPrefixOf` input = Right (Boolean True)
    | "false" `isPrefixOf` input = Right (Boolean False)
    | otherwise = Left "No boolean found"

parseNumber :: String -> Either String AstNode
parseNumber input = case span isDigit input of
    ("", _) -> Left "No number found"
    (numStr, _) -> Right (Number (read numStr))

parseString :: String -> Either String AstNode
parseString input
    | head input /= '"' = Left "String does not start with a quote"
    | otherwise = case span (/= '"') (tail input) of
        (_, "") -> Left "No closing quote found for String"
        (str, _) -> Right (String str)

parseVar :: String -> String -> [AstNode] -> Either String (String, [AstNode])
parseVar "int" code ast =
    let
        scopedCode = words (takeWhile isNotSemiColon code)
        name = head scopedCode
        value = removeSemi (unwords (drop 2 scopedCode))
        restOfCode = dropWhile isSpace (drop 1 (dropWhile isNotSemiColon code))
     in
        if "(" `isInfixOf` value
            then case parseFunctionCall value of
                Right (_, funcCall) -> Right (restOfCode, ast ++ [Operator "=" (String name) funcCall])
                Left err -> Left err
            else case parseNumber value of
                Right number -> Right (restOfCode, ast ++ [Operator "=" (String name) number])
                Left err -> Left err
parseVar "bool" code ast =
    let
        scopedCode = words (takeWhile isNotSemiColon code)
        name = head scopedCode
        value = removeSemi (unwords (drop 2 scopedCode))
        restOfCode = dropWhile isSpace (drop 1 (dropWhile isNotSemiColon code))
     in
        if "(" `isInfixOf` value
            then case parseFunctionCall value of
                Right (_, funcCall) -> Right (restOfCode, ast ++ [Operator "=" (String name) funcCall])
                Left err -> Left err
            else case parseBool value of
                Right bool -> Right (restOfCode, ast ++ [Operator "=" (String name) bool])
                Left err -> Left err
parseVar "str" code ast =
    let
        scopedCode = words (takeWhile isNotSemiColon code)
        name = head scopedCode
        (value, dropWhile isSpace . drop 1 -> restOfCode) = extractQuotedStringAndRest code
     in
        if "(" `isInfixOf` value && head value /= '"'
            then case parseFunctionCall value of
                Right (_, funcCall) -> Right (restOfCode, ast ++ [Operator "=" (String name) funcCall])
                Left err -> Left err
            else case parseString value of
                Right str -> Right (restOfCode, ast ++ [Operator "=" (String name) str])
                Left err -> Left err
parseVar _ _ _ = Left "Unrecognized variable type"

parseFunction :: String -> [AstNode] -> Either String (String, [AstNode])
parseFunction code ast =
    let (name, params, body, restOfCode) = extractFunctionParts code
     in case parseDreamberd body [] of
            Right outputAst -> Right (restOfCode, ast ++ [Function name params outputAst])
            Left err -> Left err

parseFunctionCall :: String -> Either String (String, AstNode)
parseFunctionCall code =
    let (name, rest) = break (== '(') code
        strippedName = filter (not . isSpace) name
     in if null rest
            then Left "Invalid function call"
            else
                let paramsAndRest = init $ tail rest
                    (params, remaining) = span (/= ')') paramsAndRest
                    afterParams = dropWhile (\c -> c == ';' || isSpace c) $ tail remaining
                    paramList = words $ map (\c -> if c == ',' then ' ' else c) params
                 in Right (afterParams, Call strippedName (map String paramList))

parseElement :: String -> [AstNode] -> Either String (String, [AstNode])
parseElement (stripPrefix "int" -> Just restCode) ast =
    case parseVar "int" restCode ast of
        Right (remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
        Left err -> Left err
parseElement (stripPrefix "bool" -> Just restCode) ast =
    case parseVar "bool" restCode ast of
        Right (remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
        Left err -> Left err
parseElement (stripPrefix "str" -> Just restCode) ast =
    case parseVar "str" restCode ast of
        Right (remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
        Left err -> Left err
parseElement (stripPrefix "function" -> Just restCode) ast =
    case parseFunction restCode ast of
        Right (remainingCode, updatedAst) -> Right (remainingCode, updatedAst)
        Left err -> Left err
parseElement code ast = case parseFunctionCall code of
    Right (remainingCode, call) -> Right (remainingCode, ast ++ [call])
    Left _ -> Left "Unrecognized element"

parseDreamberd :: String -> [AstNode] -> Either String [AstNode]
parseDreamberd "\n" ast = Right ast
parseDreamberd "" ast = Right ast
parseDreamberd sourceCode ast =
    case parseElement (dropWhile isSpace sourceCode) ast of
        Right (remainingCode, updatedAst) -> parseDreamberd remainingCode updatedAst
        Left err -> Left err
