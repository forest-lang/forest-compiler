{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
module Arbitrary
  (
  ) where

import Language
import HaskellSyntax

import Control.Monad
import qualified Data.List.NonEmpty as NE
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

instance Arbitrary Module where
  arbitrary = genModule
  shrink = genericShrink

instance Arbitrary Expression where
  arbitrary = genExpression
  shrink = genericShrink

instance Arbitrary OperatorExpr where
  arbitrary = genOperator
  shrink = genericShrink

instance Arbitrary Declaration where
  arbitrary = genDeclaration
  shrink = genericShrink

instance Arbitrary Annotation where
  arbitrary = genAnnotation
  shrink = genericShrink

instance Arbitrary Ident where
  arbitrary = genIdent
  shrink (Ident s) = Ident <$> filter permittedWord (shrink s)

permittedWord :: NonEmptyString -> Bool
permittedWord (NonEmptyString s) = NE.toList s `notElem` rws

instance Arbitrary NonEmptyString where
  arbitrary = genNEString
  shrink (NonEmptyString s) = NonEmptyString <$> shrinkNonEmpty s

instance Arbitrary (NE.NonEmpty Declaration) where
  arbitrary = genNonEmpty genDeclaration
  shrink = shrinkNonEmpty

instance Arbitrary (NE.NonEmpty (Expression, Expression)) where
  arbitrary = genNonEmpty genCaseBranch
  shrink = shrinkNonEmpty

instance Arbitrary (NE.NonEmpty Ident) where
  arbitrary = genNonEmpty genIdent
  shrink = shrinkNonEmpty

genModule :: Gen Module
genModule = Module <$> listOf1 genDeclaration

genNonEmpty :: Gen a -> Gen (NE.NonEmpty a)
genNonEmpty gen = NE.fromList <$> listOf1 gen

shrinkNonEmpty :: Arbitrary a => NE.NonEmpty a -> [NE.NonEmpty a]
shrinkNonEmpty n =
  let list = NE.toList n
      possibilities = shrink list
      nonEmptyPossibilities = filter (not . null) possibilities
   in map NE.fromList nonEmptyPossibilities

genExpression :: Gen Expression
genExpression =
  frequency
    [ (90, genIdentifier)
    , (90, genNumber)
    , (90, genString)
    , (10, genInfix)
    , (10, genCall)
    , (1, genLet)
    , (1, genCase)
    ]

genChar :: Gen Char
genChar = elements (['a' .. 'z'] ++ ['A' .. 'Z'])

genIdent :: Gen Ident
genIdent = Ident <$> suchThat genNEString permittedWord

genNEString :: Gen NonEmptyString
genNEString = NonEmptyString . NE.fromList <$> listOf1 genChar

genIdentifier :: Gen Expression
genIdentifier = Identifier <$> genIdent

genNumber :: Gen Expression
genNumber = Number <$> arbitrarySizedNatural

genString :: Gen Expression
genString = String' <$> listOf genChar

genDeclaration :: Gen Declaration
genDeclaration = do
  name <- genIdent
  annotation <- genMaybe genAnnotation
  args <- listOf genIdent
  expr <- genExpression
  return $ Declaration annotation name args expr

genAnnotation :: Gen Annotation
genAnnotation = do
  name <- genIdent
  types <- genNonEmpty genIdent
  return $ Annotation name types

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = oneof [Just <$> g, Nothing <$ g]

genOperator :: Gen OperatorExpr
genOperator = elements [Add, Subtract, Multiply, Divide]

genInfix :: Gen Expression
genInfix = do
  operator <- genOperator
  a <- genNumber
  b <- genExpression
  return $ BetweenParens $ Infix operator a b

genCall :: Gen Expression
genCall = do
  name <- genIdent
  args <- listOf1 genIdentifier
  return $ Call name args

(>*<) :: Gen a -> Gen b -> Gen (a, b)
x >*< y = liftM2 (,) x y

genCase :: Gen Expression
genCase = do
  caseExpr <- genExpression
  cases <- genNonEmpty genCaseBranch
  return $ Case caseExpr cases

genCaseBranch :: Gen (Expression, Expression)
genCaseBranch = oneof [genNumber, genIdentifier] >*< genExpression

genLet :: Gen Expression
genLet = do
  declarations <- genNonEmpty genDeclaration
  expr <- genExpression
  return $ Let declarations expr
