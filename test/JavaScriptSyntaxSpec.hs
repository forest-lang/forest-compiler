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
      expected <- readFixture "js/simple"
      let code =
            Module [Function $ Declaration Nothing (ne "test") [] (Number 1)]
       in printModule code `shouldBe` expected
    it "prints a function with many arguments" $ do
      expected <- readFixture "js/arguments"
      let code =
            Module
              [ Function $
                Declaration
                  Nothing
                  (ne "test")
                  [ne "a", ne "b"]
                  (Infix Add (Identifier (ne "a")) (Identifier (ne "b")))
              ]
       in printModule code `shouldBe` expected
    it "prints a function that returns a string" $ do
      expected <- readFixture "js/string"
      let code =
            Module
              [Function $ Declaration Nothing (ne "test") [] (String' "hey")]
       in printModule code `shouldBe` expected
    it "prints a function call with arguments" $ do
      expected <- readFixture "js/call-with-arguments"
      let code =
            Module
              [ Function $
                Declaration
                  Nothing
                  (ne "test")
                  [ne "a", ne "b"]
                  (Call
                     (ne "func")
                     [(Identifier (ne "a")), (Identifier (ne "b"))])
              ]
       in printModule code `shouldBe` expected
    it "prints a function with an expression inside of parens" $ do
      expected <- readFixture "js/parens"
      let code =
            Module
              [ Function $
                Declaration Nothing (ne "test") [] (BetweenParens (Number 1))
              ]
       in printModule code `shouldBe` expected
    it "prints a function with a case" $ do
      expected <- readFixture "js/case"
      let code =
            Module
              [ Function $
                Declaration
                  Nothing
                  (ne "test")
                  [ne "a"]
                  (Case
                     (Identifier (ne "a"))
                     [ (String' "Foo", String' "Bar")
                     , (String' "Ahh", String' "Woo")
                     ])
              ]
       in printModule code `shouldBe` expected

ne :: String -> Ident
ne = Ident . NonEmptyString . NE.fromList

readFixture :: String -> IO String
readFixture name = readFile ("test/fixtures/" ++ name ++ ".tree")
