{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module JavaScriptSyntaxSpec
  ( javaScriptSyntaxSpecs
  ) where

import Control.Monad
import qualified Data.List.NonEmpty as NE
import System.Exit
import System.IO.Temp
import System.Process
import Test.Hspec
import Test.QuickCheck

import Arbitrary

import Compiler
import JavaScriptSyntax
import Language

javaScriptSyntaxSpecs :: SpecWith ()
javaScriptSyntaxSpecs =
  describe "Forest JavaScript syntax" $ do
    it "prints a simple program" $ do
      let code =
            Module [Function $ Declaration Nothing (ne "test") [] (Number 1)]
          expected = "function test() { return 1 }"
       in printModule code `shouldBe` expected

ne :: String -> Ident
ne = Ident . NonEmptyString . NE.fromList

readFixture :: String -> IO String
readFixture name = readFile ("test/fixtures/" ++ name ++ ".tree")
