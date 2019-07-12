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

disallowGenericCoercion :: Text
disallowGenericCoercion =
  [r|
data List a
  = Cons a (List a)
  | Empty

add :: Int -> Int -> Int
add a b = a + b

main :: a -> b -> Int
main a b = add a 5
|]

sumOfInts :: Text
sumOfInts =
  [r|
data List a
  = Cons a (List a)
  | Empty

sum :: List Int -> Int
sum l =
  case l of
    Cons x xs -> x + sum xs
    Empty -> 0
|]

recursiveList :: Text
recursiveList =
  [r|
data List
  = Cons Int List
  | Empty
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
  let checkResult r =
        case r of
          Right m -> () <$ checkModule m
          Left err -> error $ "Failed to parse module: " ++ show err
   in parallel $ describe "Type checker" $ do
        it "checks valid expressions" $
          checkResult (parseModule valid) `shouldBe` Right ()
        it "checks valid expressions that use locals" $
          checkResult (parseModule local) `shouldBe` Right ()
        it "checks invalid expressions" $
          messages (checkResult (parseModule invalid)) `shouldBe`
          [ "Function expected argument of type Int, but instead got argument of type String"
          ]
        it "fails if a function has an incorrect return type" $
          messages (checkResult (parseModule wrongReturnType)) `shouldBe`
          ["Expected foo to return type Int, but instead got type String"]
        it "fails if a case has branches that return different types" $
          messages (checkResult (parseModule badCase)) `shouldBe`
          ["Case expression has multiple return types: String, Int"]
        it "passes with a valid case" $
          checkResult (parseModule goodCase) `shouldBe` Right ()
        it "fails if a let has incorrect types" $
          messages (checkResult (parseModule badLet)) `shouldBe`
          ["No function exists with type Int + String"]
        it "passes with a valid let" $
          checkResult (parseModule goodLet) `shouldBe` Right ()
        it "passes with a valid let that uses functions" $
          checkResult (parseModule goodFunctionLet) `shouldBe` Right ()
        xit "is insensitive to the order of declarations" $
          checkResult (parseModule unorderedDeclarations) `shouldBe` Right ()
        it "typechecks adt constructors" $
          checkResult (parseModule adt) `shouldBe` Right ()
        it "is permissive enough to express recurive sum on lists" $
          checkResult (parseModule sumOfInts) `shouldBe` Right ()
        describe "generics" $ do
          it "disallows coercion of generic types" $
            checkResult (parseModule disallowGenericCoercion) `shouldBe`
            Left
              (CompileError
                 (ExpressionError
                    (Language.Apply
                       (Language.Identifier (Ident (NonEmptyString 'a' "dd")))
                       (Language.Identifier (Ident (NonEmptyString 'a' "")))))
                 "Function expected argument of type Int, but instead got argument of type a" :|
               [])
        describe "recursive types" $ do
          it "typechecks types that refer to themselves" $
            checkResult (parseModule recursiveList) `shouldBe` Right ()
