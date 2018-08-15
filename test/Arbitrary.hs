{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module Arbitrary
  (
  ) where

import HaskellSyntax
import Language

import Control.Monad
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

instance Arbitrary Module where
  arbitrary = genModule
  shrink = genericShrink

instance Arbitrary TopLevel where
  arbitrary = genTopLevel
  shrink = genericShrink

instance Arbitrary ADT where
  arbitrary = genADT
  shrink = genericShrink

instance Arbitrary Expression where
  arbitrary = genExpression
  shrink = genericShrink

instance Arbitrary Constructor where
  arbitrary = genConstructor
  shrink = genericShrink

instance Arbitrary ConstructorType where
  arbitrary = genConstructorType
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

instance Arbitrary AnnotationType where
  arbitrary = genAnnotationType
  shrink = genericShrink

instance Arbitrary Ident where
  arbitrary = genIdent
  shrink (Ident s) = Ident <$> filter permittedWord (shrink s)

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary
  shrink s = T.pack <$> shrink (T.unpack s)

permittedWord :: NonEmptyString -> Bool
permittedWord (NonEmptyString x xs) = T.singleton x <> xs `notElem` rws

instance Arbitrary NonEmptyString where
  arbitrary = genNEString
  shrink (NonEmptyString x xs) =
    NonEmptyString x <$> (T.pack <$> shrink (T.unpack xs))

instance Arbitrary (NE.NonEmpty Declaration) where
  arbitrary = genNonEmpty genDeclaration
  shrink = shrinkNonEmpty

instance Arbitrary (NE.NonEmpty (Argument, Expression)) where
  arbitrary = genNonEmpty genCaseBranch
  shrink = shrinkNonEmpty

instance Arbitrary Argument where
  arbitrary = genArgument
  shrink = genericShrink

instance Arbitrary (NE.NonEmpty Ident) where
  arbitrary = genNonEmpty genIdent
  shrink = shrinkNonEmpty

instance Arbitrary (NE.NonEmpty Constructor) where
  arbitrary = genNonEmpty genConstructor
  shrink = shrinkNonEmpty

instance Arbitrary (NE.NonEmpty AnnotationType) where
  arbitrary = genNonEmpty genAnnotationType
  shrink = shrinkNonEmpty

genModule :: Gen Module
genModule = Module <$> listOf1 genTopLevel

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
    , (50, BetweenParens <$> genExpression)
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
genNEString = NonEmptyString <$> genChar <*> (T.pack <$> listOf1 genChar)

genIdentifier :: Gen Expression
genIdentifier = Identifier <$> genIdent

genNumber :: Gen Expression
genNumber = Number <$> arbitrarySizedNatural

genString :: Gen Expression
genString = String' . T.pack <$> listOf genChar

genTopLevel :: Gen TopLevel
genTopLevel = oneof [genFunction, genDataType]
  where
    genFunction = Function <$> genDeclaration
    genDataType = DataType <$> genADT

genADT :: Gen ADT
genADT = do
  name <- genIdent
  generics <- listOf genIdent
  constructors <- genNonEmpty genConstructor
  return $ ADT name generics constructors

genConstructor :: Gen Constructor
genConstructor = do
  name <- genIdent
  types <- genMaybe genConstructorType
  return $ Constructor name types

genConstructorType :: Gen ConstructorType
genConstructorType = frequency [(100, concrete), (100, parens), (1, applied)]
  where
    concrete = CTConcrete <$> genIdent
    applied =
      CTApplied <$> (genConstructorType `suchThat` noApply) <*>
      genConstructorType
    parens = CTParenthesized <$> genConstructorType
    noApply ct =
      case ct of
        CTApplied _ _ -> False
        _ -> True

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
  types <- genNonEmpty genAnnotationType
  return $ Annotation name types

genAnnotationType :: Gen AnnotationType
genAnnotationType =
  frequency
    [ (100, Concrete <$> genIdent)
    , (1, Parenthesized <$> genNonEmpty genAnnotationType)
    ]

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = oneof [Just <$> g, Nothing <$ g]

genOperator :: Gen OperatorExpr
genOperator = elements [Add, Subtract, Multiply, Divide]

genInfix :: Gen Expression
genInfix = do
  operator <- genOperator
  a <- genNumber -- TODO expand this definition
  b <- genExpression `suchThat` applicationIsExcluded
  return $ BetweenParens $ Infix operator a b

genCall :: Gen Expression
genCall = do
  a <-
    oneof
      [ genIdentifier
      , genNumber
      , genString
      , genCall
      , BetweenParens <$> genExpression
      ]
  b <-
    oneof
      [ genIdentifier
      , genNumber
      , genString
      , BetweenParens <$> genExpression `suchThat` applicationIsExcluded
      ]
  return $ Apply a b

applicationIsExcluded :: Expression -> Bool
applicationIsExcluded e =
  case e of
    Apply _ _ -> False
    _ -> True

(>*<) :: Gen a -> Gen b -> Gen (a, b)
x >*< y = liftM2 (,) x y

genCase :: Gen Expression
genCase = do
  caseExpr <- genExpression
  cases <- genNonEmpty genCaseBranch
  return $ Case caseExpr cases

genArgument :: Gen Argument
genArgument =
  oneof
    [ ANumberLiteral <$> arbitrarySizedNatural
    , AIdentifier <$>
      (Ident <$> genNEString `suchThat` (not . firstLetterIsCapitalized))
    ]
  where
    firstLetterIsCapitalized (NonEmptyString x _) =
      T.singleton x == (T.toUpper . T.singleton $ x)

genCaseBranch :: Gen (Argument, Expression)
genCaseBranch = genArgument >*< genExpression

genLet :: Gen Expression
genLet = do
  declarations <- genNonEmpty genDeclaration
  expr <- genExpression
  return $ Let declarations expr
