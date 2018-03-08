module Compiler
  ( compile
  , printHaskellSyntax
  ) where

import HaskellSyntax
import WASM

compile :: String -> Either ParseError' String
compile s = printWasm . forestModuleToWasm <$> parseExpressionFromString s

printHaskellSyntax :: String -> Either ParseError' String
printHaskellSyntax s = printModule <$> parseExpressionFromString s
