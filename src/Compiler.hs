module Compiler
  ( compile
  , format
  ) where

import HaskellSyntax
import WASM

compile :: String -> Either ParseError' String
compile s = printWasm . forestModuleToWasm <$> parseModule s

format :: String -> Either ParseError' String
format s = printModule <$> parseModule s
