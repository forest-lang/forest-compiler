{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.List (intercalate)
import Data.List.NonEmpty (toList)
import Data.Maybe
import Safe
import System.Environment
import Text.Megaparsec.Error

import Compiler
import HaskellSyntax
import TypeChecker

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["build", filename] -> do
      contents <- readFile filename
      putStrLn $
        case compile contents of
          Success w -> w
          ParseErr err -> reportParseError filename contents err
          CompileErr errors -> (intercalate "\n\n-----------\n\n" . toList $ printError <$> errors) ++ "\n"
    ["format", filename] -> build format filename
    ["check", filename] -> do
      contents <- readFile filename
      putStrLn $
        case check contents of
          Success _ -> "Compiled successfully"
          ParseErr err -> reportParseError filename contents err
          CompileErr errors -> (intercalate "\n\n-----------\n\n" . toList $ printError <$> errors) ++ "\n"
    _ -> putStrLn "please provide a file to compile"
  where
    build f filename = do
      contents <- readFile filename
      case f contents of
        Right a -> putStrLn a
        Left err -> putStrLn $ reportParseError filename contents err
    printError (CompileError error message) =
      case error of
        ExpressionError expr -> "Encountered a type error in an expression:\n\n" ++ indent2 (printExpression expr) ++ "\n\n" ++ message
        DeclarationError decl -> "Encountered a type error in a declaration:\n\n" ++ indent2 (printDeclaration decl) ++ "\n\n" ++ message

reportParseError :: String -> String -> ParseError' -> String
reportParseError filename contents err =
  "Syntax error in " ++ filename ++ "\n" ++ parseErrorPretty' contents err
