{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module HaskellSyntaxSpec
  ( haskellSyntaxSpecs
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
import HaskellSyntax
import Language

propParseAndPrint :: Module -> Bool
propParseAndPrint m =
  let printedModule = printModule m
      parsedModule = parseModule printedModule
   in case parsedModule of
        Right newModule -> newModule == m
        Left _ -> False

haskellSyntaxSpecs :: SpecWith ()
haskellSyntaxSpecs =
  describe "Forest haskell syntax" $ do
    it "prints and reparses arbitrary expressions losslessly" $
      withMaxSuccess 100 (property propParseAndPrint)
    it "parses a module with multple assignments" $ do
      code <- readFixture "multiple-assignments"
      let parseResult = parseModule code
      let expected =
            Module
              [ Function $
                Declaration
                  Nothing
                  Nothing
                  (ne "double")
                  [ne "a"]
                  (Infix Multiply (Identifier (ne "a")) (Number 2))
              , Function $
                Declaration
                  Nothing
                  Nothing
                  (ne "half")
                  [ne "a"]
                  (Infix Divide (Identifier (ne "a")) (Number 2))
              ]
      parseResult `shouldBe` Right expected
    it "parses an assignment with a case statement" $ do
      code <- readFixture "case-statement"
      let parseResult = parseModule code
      let expected =
            Module
              [ Function $
                Declaration
                  Nothing
                  Nothing
                  (ne "test")
                  [ne "n"]
                  (Case
                     (Identifier (ne "n"))
                     [ (Number 0, Number 1)
                     , (Number 1, Number 1)
                     , ( Identifier (ne "n")
                       , Infix Add (Identifier (ne "n")) (Number 1))
                     ])
              ]
      parseResult `shouldBe` Right expected
    it
      "parses an assignment with a case statement followed by another assignment" $ do
      code <- readFixture "case-statement-and-more"
      let parseResult = parseModule code
      let expected =
            Module
              [ Function $
                Declaration
                  Nothing
                  Nothing
                  (ne "test")
                  [ne "n"]
                  (Case
                     (Identifier (ne "n"))
                     [ (Number 0, Number 1)
                     , (Number 1, Number 1)
                     , (Identifier (ne "n"), Identifier (ne "n"))
                     ])
              , Function $
                Declaration
                  Nothing
                  Nothing
                  (ne "double")
                  [ne "x"]
                  (Infix Multiply (Identifier (ne "x")) (Number 2))
              ]
      parseResult `shouldBe` Right expected
    it "parses let expressions" $ do
      code <- readFixture "let"
      let parseResult = parseModule code
      let expected =
            Module
              [ Function $
                Declaration
                  Nothing
                  Nothing
                  (ne "a")
                  []
                  (Let
                     (NE.fromList
                        [ Declaration Nothing Nothing (ne "foo") [] (Number 5)
                        , Declaration Nothing Nothing (ne "bar") [] (Number 10)
                        ])
                     (Infix Add (Identifier (ne "foo")) (Identifier (ne "bar"))))
              ]
      parseResult `shouldBe` Right expected

ne :: String -> Ident
ne = Ident . NonEmptyString . NE.fromList

readFixture :: String -> IO String
readFixture name = readFile ("test/fixtures/" ++ name ++ ".tree")
