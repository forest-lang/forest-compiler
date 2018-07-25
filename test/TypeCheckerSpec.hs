{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeCheckerSpec
  ( typeCheckerSpecs
  ) where

import Data.Either
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
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

local :: String
local =
  [r|
add :: Int -> Int -> Int
add a b = a + b

addOne :: Int -> Int
addOne n =
  add n 1
|]

wrongReturnType :: String
wrongReturnType =
  [r|
foo :: Int
foo = "test"
|]

badCase :: String
badCase =
  [r|
main :: Int
main =
  case 5 of
    1 -> "Test"
    2 -> 2
|]

goodCase :: String
goodCase =
  [r|
main :: Int -> Int
main i =
  case i of
    1 -> 1
    2 -> 2
    i -> 5
|]

badLet :: String
badLet =
  [r|
main :: Int
main =
  let
    a :: Int
    a = 5

    b :: String
    b = "test"
  in
    a + b
|]

goodLet :: String
goodLet =
  [r|
main :: Int
main =
  let
    a :: Int
    a = 5

    b :: Int
    b = 10
  in
    a + b
|]

goodFunctionLet :: String
goodFunctionLet =
  [r|
main :: Int
main =
  let
    one :: Int
    one = 1

    addOne :: Int -> Int
    addOne n = n + one
  in
    addOne 10
|]

unorderedDeclarations :: String
unorderedDeclarations =
  [r|
main :: Int
main = foo

foo :: Int
foo = 5
|]

messages :: Either (NonEmpty CompileError) () -> [String]
messages r =
  case r of
    Right () -> []
    Left errors -> NE.toList $ m <$> errors
  where
    m (CompileError _ message) = message

typeCheckerSpecs :: SpecWith ()
typeCheckerSpecs =
  describe "Type checker" $ do
    it "checks valid expressions" $
      let moduleResult = parseModule valid
          checkResult =
            case moduleResult of
              Right m -> () <$ checkModule m
              Left err -> error $ "Failed to parse module: " ++ show err
       in checkResult `shouldBe` Right ()
    it "checks valid expressions that use locals" $
      let moduleResult = parseModule local
          checkResult =
            case moduleResult of
              Right m -> () <$ checkModule m
              Left err -> error $ "Failed to parse module: " ++ show err
       in checkResult `shouldBe` Right ()
    it "checks invalid expressions" $
      let moduleResult = parseModule invalid
          checkResult =
            case moduleResult of
              Right m -> () <$ checkModule m
              Left err -> error $ "Failed to parse module: " ++ show err
       in messages checkResult `shouldBe`
          [ "Function expected argument of type Int, but instead got argument of type String"
          ]
    it "fails if a function has an incorrect return type" $
      let moduleResult = parseModule wrongReturnType
          checkResult =
            case moduleResult of
              Right m -> () <$ checkModule m
              Left err -> error $ "Failed to parse module: " ++ show err
       in messages checkResult `shouldBe`
          ["Expected foo to return type Int, but instead got type String"]
    it "fails if a case has branches that return different types" $
      let moduleResult = parseModule badCase
          checkResult =
            case moduleResult of
              Right m -> () <$ checkModule m
              Left err -> error $ "Failed to parse module: " ++ show err
       in messages checkResult `shouldBe`
          ["Case expression has multiple return types: String, Int"]
    it "passes with a valid case" $
      let moduleResult = parseModule goodCase
          checkResult =
            case moduleResult of
              Right m -> () <$ checkModule m
              Left err -> error $ "Failed to parse module: " ++ show err
       in checkResult `shouldBe` Right ()
    it "fails if a let has incorrect types" $
      let moduleResult = parseModule badLet
          checkResult =
            case moduleResult of
              Right m -> () <$ checkModule m
              Left err -> error $ "Failed to parse module: " ++ show err
       in messages checkResult `shouldBe`
          ["No function exists with type Int + String"]
    it "passes with a valid let" $
      let moduleResult = parseModule goodLet
          checkResult =
            case moduleResult of
              Right m -> () <$ checkModule m
              Left err -> error $ "Failed to parse module: " ++ show err
       in checkResult `shouldBe` Right ()
    it "passes with a valid let that uses functions" $
      let moduleResult = parseModule goodFunctionLet
          checkResult =
            case moduleResult of
              Right m -> () <$ checkModule m
              Left err -> error $ "Failed to parse module: " ++ show err
       in checkResult `shouldBe` Right ()
    it "is insensitive to the order of declarations" $
      let moduleResult = parseModule unorderedDeclarations
          checkResult =
            case moduleResult of
              Right m -> () <$ checkModule m
              Left err -> error $ "Failed to parse module: " ++ show err
       in checkResult `shouldBe` Right ()
