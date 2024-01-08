{-# LANGUAGE ViewPatterns #-}

module Dreamberd.Parsing.Elements.Condition (parseConditionParts, parseConditionExpression) where
    
import Data.Char (isSpace)
import Dreamberd.Parsing.Elements.Function (extractFunctionBodyAndRest)
import Dreamberd.Parsing.Elements.Operator (parseOperator)
import Dreamberd.Parsing.Values (parseAnyValue)
import Dreamberd.Types (AstNode ( Operator))

parseConditionExpression :: String -> Either String AstNode
parseConditionExpression input = 
    case parseAnyValue (dropWhile isSpace input) of
        Right lhsValue ->
            let restInput = dropWhile isSpace (dropWhile (not . isSpace) input)
            in case parseOperator restInput of
                Right (op, rest) -> 
                    case parseAnyValue rest of
                        Right rhsValue -> Right (Operator op lhsValue rhsValue)
                        Left err -> Left err
                Left err -> Left err
        Left err -> Left err



parseConditionParts :: String -> Either String (String, String, [(String, String)], Maybe (String, String), String)
parseConditionParts str =
    let (beforeIf, afterIf) = break (== '{') str
        (filter (not . isSpace) -> condition, _) = break (== ')') beforeIf
        (ifBody, restAfterIf) = extractFunctionBodyAndRest (drop 1 afterIf) 1 []
     in if null condition
            then Left "Missing condition in if statement"
            else case extractElifsAndElse restAfterIf of
                Left err -> Left err
                Right (elifs, elsePart, restOfCode) -> Right (condition, ifBody, elifs, elsePart, restOfCode)

extractElifsAndElse :: String -> Either String ([(String, String)], Maybe (String, String), String)
extractElifsAndElse str
    | take 4 str == "elif" =
        case parseConditionParts (drop 4 str) of
            Left err -> Left err
            Right (elifCondition, elifBody, elifs, elsePart, rest) ->
                if null elifCondition
                    then Left "Missing condition in elif statement"
                    else Right ((elifCondition, elifBody) : elifs, elsePart, rest)
    | take 4 str == "else" =
        let (filter (not . isSpace) -> elseKeyword, restAfterElseKeyword) = break (== '{') (drop 4 str)
            (elseBody, restOfCode) = extractFunctionBodyAndRest (drop 1 restAfterElseKeyword) 1 []
         in if not (null elseKeyword)
                then Left "Unexpected condition in else statement"
                else Right ([], Just (elseKeyword, elseBody), restOfCode)
    | otherwise = Right ([], Nothing, str)
