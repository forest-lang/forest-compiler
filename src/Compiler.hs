module Compiler
  ( compile
  , format
  , check
  , Result(..)
  ) where

import HaskellSyntax
import WASM
import TypeChecker
import Data.List.NonEmpty (NonEmpty)

data Result a
  = Success a
  | ParseErr ParseError'
  | CompileErr (NonEmpty CompileError)

check :: String -> Result ()
check s =
  case parseModule s of
    Left err -> ParseErr err
    Right mod ->
      case checkModule mod of
        Left err' -> CompileErr err'
        Right () -> Success ()

compile :: String -> Either ParseError' String
compile s = printWasm . forestModuleToWasm <$> parseModule s

format :: String -> Either ParseError' String
format s = printModule <$> parseModule s
