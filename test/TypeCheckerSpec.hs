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
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit
import System.IO.Temp
import System.Process
import Test.Hspec
import Text.RawString.QQ

import HaskellSyntax
import Language
import TypeChecker

valid :: Text
valid =
  [r|
add :: Int -> Int -> Int
add a b = a + b

main :: Int
main =
  add 1 1
|]

invalid :: Text
invalid =
  [r|
add :: Int -> Int -> Int
add a b = a + b

main :: Int
main =
  add 1 "test"
|]

local :: Text
local =
  [r|
add :: Int -> Int -> Int
add a b = a + b

addOne :: Int -> Int
addOne n =
  add n 1
|]

wrongReturnType :: Text
wrongReturnType =
  [r|
foo :: Int
foo = "test"
|]

badCase :: Text
badCase =
  [r|
main :: Int
main =
  case 5 of
    1 -> "Test"
    2 -> 2
|]

goodCase :: Text
goodCase =
  [r|
main :: Int -> Int
main i =
  case i of
    1 -> 1
    2 -> 2
    i -> 5
|]

badLet :: Text
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

goodLet :: Text
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

goodFunctionLet :: Text
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

unorderedDeclarations :: Text
unorderedDeclarations =
  [r|
main :: Int
main = foo

foo :: Int
foo = 5
|]

adt :: Text
adt =
  [r|
data Maybe a
  = Just a
  | Nothing


main :: Maybe Int
main = Just 0

nada :: Maybe Int
nada = Nothing

doubleIfFive :: Int -> Maybe Int
doubleIfFive n =
  case n of
    5 -> Just 10
    n -> Nothing

data User
  = User String

user :: User
user = User "Nick"

withDefault :: a -> Maybe a -> a
withDefault d maybe =
  case maybe of
    Just a -> a
    Nothing -> d
|]

messages :: Either (NonEmpty CompileError) () -> [Text]
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
    xit "is insensitive to the order of declarations" $
      let moduleResult = parseModule unorderedDeclarations
          checkResult =
            case moduleResult of
              Right m -> () <$ checkModule m
              Left err -> error $ "Failed to parse module: " ++ show err
       in checkResult `shouldBe` Right ()
    it "typechecks adt constructors" $
      let moduleResult = parseModule adt
          checkResult =
            case moduleResult of
              Right m -> () <$ checkModule m
              Left err -> error $ "Failed to parse module: " ++ show err
       in checkResult `shouldBe` Right ()
