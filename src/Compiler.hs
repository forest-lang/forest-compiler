module Compiler
  ( compile
  ) where

import Lib
import WASM

compile :: String -> Either Lib.ParseError' String
compile s = printWasm . forestModuleToWasm <$> parseExpressionFromString s
