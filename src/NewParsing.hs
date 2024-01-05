{-# LANGUAGE ViewPatterns #-}

module NewParsing (parseDreamberd) where

import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf, stripPrefix)
import NewTypes (AstNode (Boolean, Call, Function, Identifier, Number, Operator, Return, String))
import Text.Regex.Posix ((=~))

bannedVariables :: [String]
bannedVariables = ["if", "elif", "else", "true", "false", "return", "function", "int", "str", "bool"]

getVariableName :: String -> Either String String
getVariableName str = case str =~ "^[a-zA-Z_-]+" :: (String, String, String) of
    (_, match, _) | null match -> Left "No variable name found"
    (_, match, _) | match `elem` bannedVariables -> Left $ "Variable name " ++ show match ++ " is banned"
    (_, match, _) -> Right match

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
parseVar _ code ast =
    case getVariableName (head (words code)) of
        Right name ->
            let
                (value, restOfCode) = extractValueAndRest (unwords (drop 2 (words code)))
             in
                case parseAnyValue value of
                    Right node -> Right (restOfCode, ast ++ [Operator "=" (String name) node])
                    Left err -> Left err
        Left err -> Left err

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

extractValueAndRest :: String -> (String, String)
extractValueAndRest = go False []
  where
    go _ acc [] = (reverse acc, [])
    go inQuotes acc (x : xs)
        | x == '"' = go (not inQuotes) (x : acc) xs
        | x == ';' && not inQuotes = (dropWhile isSpace (reverse acc), xs)
        | otherwise = go inQuotes (x : acc) xs

parseAnyValue :: String -> Either String AstNode
parseAnyValue input = case parseString input of
    Right result -> Right result
    Left _ -> case parseNumber input of
        Right result -> Right result
        Left _ -> case parseBool input of
            Right result -> Right result
            Left _ -> case parseFunctionCall input of
                Right (_, result) -> Right result
                Left _ -> case getVariableName input of
                    Right name -> Right (Identifier name)
                    Left err -> Left ("Unable to parse value: " ++ err)

parseReturn :: String -> Either String (String, AstNode)
parseReturn code =
    let (value, rest) = extractValueAndRest code
     in case parseAnyValue value of
            Right result -> Right (rest, Return result)
            Left err -> Left err

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
parseElement (stripPrefix "return" -> Just restCode) ast =
    case parseReturn restCode of
        Right (remainingCode, returnNode) -> Right (remainingCode, ast ++ [returnNode])
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
