{-# LANGUAGE DeriveFunctor #-}

module Compiler
  ( compile
  , format
  , typeCheck
  , Result(..)
  , formatJS
  ) where

import Data.List.NonEmpty (NonEmpty)
import Debug.Trace (trace)
import Data.Text (Text)

import qualified JavaScriptSyntax as JS
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
  case parseModuleWithLineInformation code of
    Left parseError -> ParseErr parseError
    Right (forestModule, lineInformation) ->
      case checkModuleWithLineInformation forestModule (Just lineInformation) of
        Left compileError -> CompileErr compileError
        Right typedModule -> Success typedModule

compile :: Text -> Result Text
compile code = printWasm . forestModuleToWasm <$> typeCheck code

format :: Text -> Either ParseError' Text
format s = printModule <$> parseModule s

formatJS :: Text -> Either ParseError' Text
formatJS s = JS.printModule <$> parseModule s
