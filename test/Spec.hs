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
import SampleSpec

main :: IO ()
main = do
  vSampleSpecs <- validSampleSpecs
  iSampleSpecs <- invalidSampleSpecs
  hspec $ do
    typeCheckerSpecs
    vSampleSpecs
    iSampleSpecs
    wasmSpecs
    haskellSyntaxSpecs
