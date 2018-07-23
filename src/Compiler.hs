{-# LANGUAGE DeriveFunctor #-}

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
  deriving (Functor)

check :: String -> Result TypedModule
check s =
  case parseModule s of
    Left err -> ParseErr err
    Right mod ->
      case checkModule mod of
        Left err' -> CompileErr err'
        Right m -> Success m

compile :: String -> Result String
compile s = printWasm . forestModuleToWasm <$> check s

format :: String -> Either ParseError' String
format s = printModule <$> parseModule s
