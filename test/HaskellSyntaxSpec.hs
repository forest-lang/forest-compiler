{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module HaskellSyntaxSpec
  ( haskellSyntaxSpecs
  ) where

import Control.Monad
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit
import System.IO.Temp
import System.Process
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec

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
    it "parses a module with multiple assignments" $ do
      code <- readFixture "multiple-assignments"
      let parseResult = parseModule code
      let expected =
            Module
              [ Function $
                Declaration
                  Nothing
                  (ne "double")
                  [ne "a"]
                  (Infix Multiply (Identifier (ne "a")) (Number 2))
              , Function $
                Declaration
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
                  (ne "test")
                  [ne "n"]
                  (Case
                     (Identifier (ne "n"))
                     [ (ANumberLiteral 0, Number 1)
                     , (ANumberLiteral 1, Number 1)
                     , ( AIdentifier (ne "n")
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
                  (ne "test")
                  [ne "n"]
                  (Case
                     (Identifier (ne "n"))
                     [ (ANumberLiteral 0, Number 1)
                     , (ANumberLiteral 1, Number 1)
                     , (AIdentifier (ne "n"), Identifier (ne "n"))
                     ])
              , Function $
                Declaration
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
                  (ne "a")
                  []
                  (Let
                     (NE.fromList
                        [ Declaration Nothing (ne "foo") [] (Number 5)
                        , Declaration Nothing (ne "bar") [] (Number 10)
                        ])
                     (Infix Add (Identifier (ne "foo")) (Identifier (ne "bar"))))
              ]
      parseResult `shouldBe` Right expected
    it "parses type applications in annotations" $ do
      let code = "foo :: Int -> Maybe Int"
      let parseResult = parse annotation "" code
      let expected =
            Annotation (ne "foo") $
            Concrete (ne "Int") :|
            [TypeApplication (Concrete (ne "Maybe")) (Concrete (ne "Int"))]
      parseResult `shouldBe` Right expected
    it "parses complex type applications in annotations" $ do
      let code = "foo :: Int -> Maybe (Int -> String)"
      let parseResult = parse annotation "" code
      let expected =
            Annotation (ne "foo") $
            Concrete (ne "Int") :|
            [ TypeApplication
                (Concrete (ne "Maybe"))
                (Parenthesized (Concrete (ne "Int") :| [Concrete (ne "String")]))
            ]
      parseResult `shouldBe` Right expected
    it "parses complex type applications in adt constructors" $ do
      let code = "data Foo a\n= Foo (Maybe a)"
      let parseResult = parse dataType "" code
      let expected =
            DataType $
            ADT
              (ne "Foo")
              [ne "a"]
              [ Constructor (ne "Foo") $
                Just
                  (CTParenthesized
                     (CTApplied (CTConcrete (ne "Maybe")) (CTConcrete (ne "a"))))
              ]
      parseResult `shouldBe` Right expected
    it "parses adt deconstructions in cases " $ do
      code <- readFixture "case-deconstruction"
      let parseResult = parseModule code
      let expected =
            Module
              [ DataType
                  (ADT
                     (ne "Maybe")
                     [ne "a"]
                     (Constructor (ne "Just") (Just (CTConcrete (ne "a"))) :|
                      [Constructor (ne "Nothing") Nothing]))
              , Function
                  (Declaration
                     (Just
                        (Annotation
                           (ne "main")
                           (TypeApplication
                              (Concrete (ne "Maybe"))
                              (Concrete (ne "Int")) :|
                            [Concrete (ne "Int")])))
                     (ne "main")
                     [ne "m"]
                     (Case
                        (Identifier (ne "m"))
                        ((ADeconstruction (ne "Just") [AIdentifier (ne "n")], Identifier (ne "n")) :|
                         [(ADeconstruction (ne "Nothing") [], Number 0)])))
              ]
      parseResult `shouldBe` Right expected
    it "prints and reparses arbitrary expressions losslessly" $
      withMaxSuccess 100 (property propParseAndPrint)

ne :: Text -> Ident
ne s = Ident $ NonEmptyString (T.head s) (T.tail s)

readFixture :: Text -> IO Text
readFixture name = TIO.readFile ("test/fixtures/" <> T.unpack name <> ".tree")
