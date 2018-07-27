{-# LANGUAGE DeriveFunctor #-}

module Compiler
  ( compile
  , format
  , check
  , Result(..)
  ) where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)

import HaskellSyntax
import Wasm
import TypeChecker

data Result a
  = Success a
  | ParseErr ParseError'
  | CompileErr (NonEmpty CompileError)
  deriving (Functor)

check :: Text -> Result TypedModule
check s =
  case parseModule s of
    Left err -> ParseErr err
    Right mod ->
      case checkModule mod of
        Left err' -> CompileErr err'
        Right m -> Success m

compile :: Text -> Result Text
compile s = printWasm . forestModuleToWasm <$> check s

format :: Text -> Either ParseError' Text
format s = printModule <$> parseModule s
