{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.List.NonEmpty (toList)
import Data.Maybe
import Data.Semigroup
import Data.Text
import qualified Data.Text.IO as TIO
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
      contents <- TIO.readFile filename
      TIO.putStrLn $
        case compile contents of
          Success w -> w
          ParseErr err -> reportParseError filename contents err
          CompileErr errors ->
            (intercalate "\n\n-----------\n\n" . toList $ printError <$> errors) <>
            "\n"
    ["format", filename] -> build format filename
    ["check", filename] -> do
      contents <- TIO.readFile filename
      TIO.putStrLn $
        case check contents of
          Success _ -> "🎉  no errors found 🎉"
          ParseErr err -> reportParseError filename contents err
          CompileErr errors ->
            (intercalate "\n\n-----------\n\n" . toList $ printError <$> errors) <>
            "\n"
    _ -> TIO.putStrLn "please provide a file to compile"
  where
    build :: (Text -> Either ParseError' Text) -> String -> IO ()
    build f filename = do
      contents <- TIO.readFile filename
      case f contents of
        Right a -> TIO.putStrLn a
        Left err -> TIO.putStrLn $ reportParseError filename contents err
    printError (CompileError error message) =
      case error of
        ExpressionError expr ->
          "Encountered a type error in an expression:\n\n" <>
          indent2 (printExpression expr) <>
          "\n\n" <>
          message
        DeclarationError decl ->
          "Encountered a type error in a declaration:\n\n" <>
          indent2 (printDeclaration decl) <>
          "\n\n" <>
          message

reportParseError :: String -> Text -> ParseError' -> Text
reportParseError filename contents err =
  "Syntax error in " <> pack filename <> "\n" <>
  pack (parseErrorPretty' (unpack contents) err)
