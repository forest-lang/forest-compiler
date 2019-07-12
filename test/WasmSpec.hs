{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module WasmSpec
  ( wasmSpecs
  ) where

import Data.Either
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text, unpack)
import qualified Data.Text as T
import System.Exit
import System.IO.Temp
import System.Process
import Test.Hspec
import Test.QuickCheck
import Text.RawString.QQ

import Compiler
import HaskellSyntax
import Language
import TypeChecker
import Wasm

import Arbitrary

instance Testable (IO Bool) where
  property = ioProperty

propCodeThatTypeChecksShouldCompile :: Language.Module -> IO Bool
propCodeThatTypeChecksShouldCompile m =
  case printWasm . forestModuleToWasm <$> checkModule m of
    Right wat -> do
      path <- writeSystemTempFile "wat" (unpack wat)
      exitCode <- system $ "wat2wasm " ++ show path ++ " -o /dev/null"
      case exitCode of
        ExitSuccess -> return True
        ExitFailure _ -> do
          _ <- system "mkdir -p failures"
          writeFile "./failures/last.tree" (unpack wat)
          return False
    Left _ -> return True

wasmSpecs :: SpecWith ()
wasmSpecs =
  parallel $ describe "wasm code generation" $
  it "generates valid wasm for any well typed module" $
  withMaxSuccess
    1000
    (property (forAll genModule propCodeThatTypeChecksShouldCompile))
