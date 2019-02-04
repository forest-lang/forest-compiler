{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.List.NonEmpty (toList)
import Data.Maybe
import Data.Semigroup
import Data.Text
import qualified Data.Text.IO as TIO
import Safe
import System.Environment
import System.Exit
import System.IO
import Text.Megaparsec.Error
import Text.RawString.QQ

import Compiler
import HaskellSyntax
import TypeChecker

usage :: Text
usage =
  strip
    [r|
usage: forest command path

commands:

  build - typechecks and compiles the given file to Wast
  format - format and print the given file
  check - typechecks the given file
|]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["build", filename] -> do
      contents <- TIO.readFile filename
      let (printText, exitCode) =
            case compile contents of
              Success w -> (TIO.putStrLn w, ExitSuccess)
              ParseErr err ->
                (TIO.hPutStrLn stderr $ reportParseError filename contents err, ExitFailure 1)
              CompileErr errors ->
                (TIO.hPutStrLn stderr $  (intercalate "\n\n-----------\n\n" . toList $
                   printError <$> errors) <>
                  "\n"
                , ExitFailure 2)
      printText >> exitWith exitCode
    ["format", filename] -> do
      contents <- TIO.readFile filename
      case format contents of
        Right a ->
          TIO.writeFile filename a >> TIO.putStrLn "Formatted successfully."
        Left err ->
          (TIO.hPutStrLn stderr $ reportParseError filename contents err) >>
          exitWith (ExitFailure 1)
    ["check", filename] -> do
      contents <- TIO.readFile filename
      let (printText, exitCode) =
            case check contents of
              Success _ -> (TIO.putStrLn "No errors found.", ExitSuccess)
              ParseErr err ->
                (TIO.hPutStrLn stderr $ reportParseError filename contents err, ExitFailure 1)
              CompileErr errors ->
                ( TIO.hPutStrLn stderr $ (intercalate "\n\n-----------\n\n" . toList $
                   printError <$> errors) <>
                  "\n"
                , ExitFailure 2)
      printText >> exitWith exitCode
    _ -> TIO.hPutStrLn stderr usage >> exitFailure
  where
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
        DataTypeError dt ->
          "Encountered a type error in a datatype:\n\n" <>
          indent2 (printDataType dt) <>
          "\n\n" <>
          message

reportParseError :: String -> Text -> ParseError' -> Text
reportParseError filename contents err =
  "Syntax error in " <> pack filename <> "\n" <>
  pack (parseErrorPretty' (unpack contents) err)
