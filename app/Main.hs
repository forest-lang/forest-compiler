{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Data.ByteString as BS
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Maybe
import Data.Semigroup
import Data.Text (Text, intercalate, pack, strip, unpack)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Rainbow hiding ((<>))
import Safe
import System.Environment
import System.Exit
import System.IO
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Error
import Text.RawString.QQ

import Compiler
import HaskellSyntax
import TypeChecker

showT :: Show a => a -> Text
showT = Text.pack . show

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
              Success compiledWast -> (TIO.putStrLn compiledWast, ExitSuccess)
              ParseErr err ->
                ( TIO.hPutStrLn stderr $ reportParseError filename err
                , ExitFailure 1)
              CompileErr errors ->
                let errorChunks :: [[Chunk Text]]
                    errorChunks = toList $ printError contents <$> errors
                    divider = [chunk "\n\n-----------\n\n"]
                    chunks =
                      intersperse divider errorChunks <>
                      [[chunk ("\n" :: Text)]]
                 in (printChunks $ concat chunks, ExitFailure 2)
      printText >> exitWith exitCode
    ["format", filename] -> do
      contents <- TIO.readFile filename
      case format contents of
        Right formattedCode ->
          TIO.writeFile filename formattedCode >>
          TIO.putStrLn "Formatted successfully."
        Left err ->
          (TIO.hPutStrLn stderr $ reportParseError filename err) >>
          exitWith (ExitFailure 1)
    ["format-js", filename] -> do
      contents <- TIO.readFile filename
      case formatJS contents of
        Right formattedCode ->
          TIO.putStrLn formattedCode
        Left err ->
          (TIO.hPutStrLn stderr $ reportParseError filename err) >>
          exitWith (ExitFailure 1)
    ["check", filename] -> do
      contents <- TIO.readFile filename
      let (printText, exitCode) =
            case typeCheck contents of
              Success _ -> (TIO.putStrLn "No errors found.", ExitSuccess)
              ParseErr err ->
                ( TIO.hPutStrLn stderr $ reportParseError filename err
                , ExitFailure 1)
              CompileErr errors ->
                let errorChunks :: [[Chunk Text]]
                    errorChunks = toList $ printError contents <$> errors
                    divider = [chunk "\n\n-----------\n\n"]
                    chunks =
                      intersperse divider errorChunks <>
                      [[chunk ("\n" :: Text)]]
                 in (printChunks $ concat chunks, ExitFailure 2)
      printText >> exitWith exitCode
    _ -> TIO.hPutStrLn stderr usage >> exitFailure
  where
    positionText p =
      case p of
        Just (start, end) ->
          Text.pack (sourcePosPretty start <> "-" <> sourcePosPretty end)
        Nothing -> ""
    printError contents (CompileError error maybeSourceRange message) =
      case error of
        ExpressionError expression ->
          case maybeSourceRange of
            Just (start, end) ->
              let contextRangeStart = unPos (sourceLine start) - 2
                  contextRangeEnd = unPos (sourceLine end) + 1
                  contentLines = Text.lines contents
                  colorLine line =
                    let (lineStart, remainder) =
                          Text.splitAt (unPos (sourceColumn start) + 3) line
                        (highlight, lineEnd) = Text.splitAt (unPos (sourceColumn end) - unPos (sourceColumn start)) remainder
                     in [ chunk lineStart
                        , chunk highlight & underline & fore brightRed
                        , chunk lineEnd
                        ]
                  color lineNumber line =
                    if lineNumber >= unPos (sourceLine start) &&
                       lineNumber <= unPos (sourceLine end)
                      then colorLine line
                      else [chunk line]
                  contextLines =
                    concatMap
                      (\(lineNumber, line) ->
                         color
                           lineNumber
                           (showT lineNumber <> " | " <> line <> "\n"))
                      (filter
                         (\(i, _) ->
                            i >= contextRangeStart && i <= contextRangeEnd)
                         (zip [1 ..] contentLines))
               in [chunk $ "Error: ", chunk $ message <> "\n"] <> contextLines
            Nothing ->
              [ chunk $
                "Encountered a type error in an expression:\n" <> "\n" <>
                indent2 (printExpression expression) <>
                "\n\n" <>
                message
              ]
        DeclarationError declaration ->
          [ chunk $
            "Encountered a type error in a declaration:\n" <>
            positionText maybeSourceRange <>
            "\n" <>
            indent2 (printDeclaration declaration) <>
            "\n\n" <>
            message
          ]
        DataTypeError dataType ->
          [ chunk $
            "Encountered a type error in a datatype:\n" <>
            positionText maybeSourceRange <>
            "\n" <>
            indent2 (printDataType dataType) <>
            "\n\n" <>
            message
          ]

printChunks :: [Chunk Text] -> IO ()
printChunks chunks = do
  printer <- byteStringMakerFromEnvironment
  mapM_ (BS.hPut stderr) . chunksToByteStrings printer $ chunks

reportParseError :: String -> ParseError' -> Text
reportParseError filename parseError =
  "Syntax error in " <> pack filename <> "\n" <>
  pack (errorBundlePretty parseError)
