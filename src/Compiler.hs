module Compiler
  ( compile
  ) where

import HaskellSyntax
import WASM

compile :: String -> Either ParseError' String
compile s = printWasm . forestModuleToWasm <$> parseExpressionFromString s
