{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeCheckerSpec
  ( typeCheckerSpecs
  ) where

import Data.Either
import Data.List.NonEmpty (NonEmpty(..))
import System.Exit
import System.IO.Temp
import System.Process
import Test.Hspec
import Text.RawString.QQ

import HaskellSyntax
import Language
import TypeChecker

valid :: String
valid =
  [r|
add :: Int -> Int -> Int
add a b = a + b

main :: Int
main =
  add 1 1
|]

invalid :: String
invalid =
  [r|
add :: Int -> Int -> Int
add a b = a + b

main :: Int
main =
  add 1 "test"
|]

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe either =
  case either of
    Left _ -> Nothing
    Right b -> Just b

typeCheckerSpecs :: SpecWith ()
typeCheckerSpecs =
  describe "Type checker" $ do
    it "checks valid expressions" $
      let
        moduleResult = parseModule valid
        checkResult =
          case moduleResult of
            Right m -> checkModule m
            Left err -> Left (CompileError ("Failed to parse module: " ++ show err) :| [])
      in
        checkResult `shouldBe` Right ()
    it "checks invalid expressions" $
      let
        moduleResult = parseModule invalid
        checkResult =
          case moduleResult of
            Right m -> checkModule m
            Left err -> Left (CompileError ("Failed to parse module: " ++ show err) :| [])
      in
        checkResult `shouldBe` Left (CompileError "Expected Num, got Str" :| [])
