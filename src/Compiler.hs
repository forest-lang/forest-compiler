{-# LANGUAGE DeriveFunctor #-}

module Compiler
  ( compile
  , format
  , typeCheck
  , Result(..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

import HaskellSyntax
import TypeChecker
import Wasm

data Result a
  = Success a
  | ParseErr ParseError'
  | CompileErr (NonEmpty CompileError)
  deriving (Show, Functor)

typeCheck :: Text -> Result TypedModule
typeCheck code =
  case parseModule code of
    Left parseError -> ParseErr parseError
    Right forestModule ->
      case checkModule forestModule of
        Left compileError -> CompileErr compileError
        Right typedModule -> Success typedModule

compile :: Text -> Result Text
compile code = printWasm . forestModuleToWasm <$> typeCheck code

format :: Text -> Either ParseError' Text
format s = printModule <$> parseModule s
