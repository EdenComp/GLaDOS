{-# LANGUAGE LambdaCase #-}

module Dreamberd.Compilation.Preprocessing (
    executePreprocessing,
) where

import Dreamberd.Parsing.Main (parseDreamberd)
import Dreamberd.Types (AstNode (..))
import System.Directory (getCurrentDirectory, getHomeDirectory)

executePreprocessing :: [AstNode] -> IO (Either String [AstNode])
executePreprocessing nodes =
    preprocessNodes nodes [] >>= \case
        Left err -> return $ Left err
        Right (_, nodes') -> return $ Right nodes'

preprocessNodes :: [AstNode] -> [String] -> IO (Either String ([String], [AstNode]))
preprocessNodes [] imports = return $ Right (imports, [])
preprocessNodes (Import path : rest) imports =
    preprocessImport path imports >>= \case
        Right (imports', nodes) ->
            preprocessNodes rest imports' >>= \case
                Right (imports'', nodes') -> return $ Right (imports'', nodes ++ nodes')
                Left err -> return $ Left err
        Left err -> return $ Left err
preprocessNodes (x : xs) imports =
    preprocessNodes xs imports >>= \case
        Right (imports', nodes) -> return $ Right (imports', x : nodes)
        Left err -> return $ Left err

preprocessImport :: String -> [String] -> IO (Either String ([String], [AstNode]))
preprocessImport ('s' : 't' : 'd' : ':' : ':' : lib) imports = do
    home <- getHomeDirectory
    file <- resolveImport ("std::" ++ lib) (home ++ "/.dreamberd/std/" ++ lib ++ ".db4") imports
    case file of
        Left err -> return $ Left err
        Right (imports', file') -> importFile ("std::" ++ lib) (imports', file')
preprocessImport lib imports = do
    current <- getCurrentDirectory
    file <- resolveImport lib (current ++ "/" ++ lib ++ ".db4") imports
    case file of
        Left err -> return $ Left err
        Right (imports', file') -> importFile lib (imports', file')

resolveImport :: String -> FilePath -> [String] -> IO (Either String ([String], String))
resolveImport lib path imports = do
    file <- readFile path
    if path `elem` imports
        then return $ Left ("Circular dependency detected: " ++ lib)
        else return $ Right (path : imports, file)

importFile :: String -> ([String], String) -> IO (Either String ([String], [AstNode]))
importFile lib (imports, code) = case parseDreamberd code [] of
    Right ast -> preprocessNodes ast imports
    Left err -> return $ Left ("Error while importing " ++ lib ++ ": " ++ err)
