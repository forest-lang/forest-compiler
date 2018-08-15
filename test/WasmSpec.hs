{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module WasmSpec
  ( wasmSpecs
  ) where

import Data.Either
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit
import System.IO.Temp
import System.Process
import Test.Hspec
import Test.QuickCheck
import Text.RawString.QQ

import HaskellSyntax
import Language
import TypeChecker
import Wasm

import Arbitrary

propCodeThatTypeChecksShouldCompile :: Language.Module -> Bool
propCodeThatTypeChecksShouldCompile m =
  case checkModule m of
    Right typedModule ->
      case m of
        Language.Module [] -> True
        _ ->
          sum
            (T.length <$> T.lines (printWasm $ forestModuleToWasm typedModule)) >
          1
    Left _ -> True

wasmSpecs :: SpecWith ()
wasmSpecs =
  describe "wasm code generation" $
  it "checks valid expressions" $
  withMaxSuccess 500 (property propCodeThatTypeChecksShouldCompile)
