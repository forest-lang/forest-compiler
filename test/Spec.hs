{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified Data.List.NonEmpty as NE
import System.Exit
import System.IO.Temp
import System.Process
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Compiler
import HaskellSyntax
import Language

import HaskellSyntaxSpec
import TypeCheckerSpec
import WasmSpec

main :: IO ()
main =
  hspec $ do
    typeCheckerSpecs
    wasmSpecs
    haskellSyntaxSpecs
