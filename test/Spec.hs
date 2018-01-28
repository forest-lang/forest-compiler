{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.List.NonEmpty as NE
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Lib

instance Arbitrary Module where
  arbitrary = genModule
  shrink = genericShrink

instance Arbitrary Expression where
  arbitrary = genExpression
  shrink = genericShrink

instance Arbitrary OperatorExpr where
  arbitrary = genOperator
  shrink = genericShrink

instance Arbitrary TopLevelDeclaration where
  arbitrary = genTopLevelDeclaration
  shrink = genericShrink

instance Arbitrary NonEmptyString where
  arbitrary = genString
  shrink (NonEmptyString s) =
    let
      charString = NE.toList s
      possibilities = shrink charString
      nonEmptyPossibilities = filter (not . null) possibilities
    in
      map (NonEmptyString . NE.fromList) nonEmptyPossibilities

genModule :: Gen Module
genModule = Module <$> listOf1 genTopLevelDeclaration

genExpression :: Gen Expression
genExpression = oneof [genIdentifier, genNumber, genAssignment, genInfix]

genChar :: Gen Char
genChar = elements (['a' .. 'z'] ++ ['A' .. 'Z'])

genString :: Gen NonEmptyString
genString = NonEmptyString . NE.fromList <$> listOf1 genChar

genIdentifier :: Gen Expression
genIdentifier = do
  name <- genString
  return $ Identifier name

genNumber :: Gen Expression
genNumber = do
  number <- arbitrarySizedNatural
  return $ Number number

genAssignment :: Gen Expression
genAssignment = do
  name <- genString
  args <- listOf genString
  expr <- genExpression
  return $ Assignment name args expr

genTopLevelDeclaration :: Gen TopLevelDeclaration
genTopLevelDeclaration = do
  name <- genString
  args <- listOf genString
  expr <- genExpression
  return $ TopLevelDeclaration name args expr

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
  name <- genString
  args <- listOf1 genIdentifier
  return $ Call name args

(>*<) :: Gen a -> Gen b -> Gen (a, b)
x >*< y = liftM2 (,) x y

genCase :: Gen Expression
genCase = do
  caseExpr <- genExpression
  cases <- listOf1 genCase
  return $ Case caseExpr cases
  where
    genCase = oneof [genNumber, genIdentifier] >*< genExpression

propParseAndPrint :: Module -> Bool
propParseAndPrint expr =
  let output = printModule expr
      reparsedExpr = parseExpressionFromString output
  in case reparsedExpr of
       Right newExpr -> newExpr == expr
       Left _ -> False

main :: IO ()
main =
  hspec $
  describe "Forest haskell syntax" $ do
    it "prints and reparses arbitrary expressions losslessly" $
      property propParseAndPrint
    it "parses a module with multple assignments" $ do
      code <- readFixture "multiple-assignments"
      let parseResult = parseExpressionFromString code
      let expected =
            Module
              [ TopLevelDeclaration
                  (ne "double")
                  [(ne "a")]
                  (Infix Multiply (Identifier (ne "a")) (Number 2))
              , TopLevelDeclaration
                  (ne "half")
                  [(ne "a")]
                  (Infix Divide (Identifier (ne "a")) (Number 2))
              ]
      parseResult `shouldBe` Right expected
    it "parses an assignment with a case statement" $ do
      code <- readFixture "case-statement"
      let parseResult = parseExpressionFromString code
      let expected =
            Module
              [ TopLevelDeclaration
                  (ne "test")
                  [(ne "n")]
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
      let parseResult = parseExpressionFromString code
      let expected =
            Module
              [ TopLevelDeclaration
                  (ne "test")
                  [(ne "n")]
                  (Case
                     (Identifier (ne "n"))
                     [ (Number 0, Number 1)
                     , (Number 1, Number 1)
                     , (Identifier (ne "n"), Identifier (ne "n"))
                     ])
              , TopLevelDeclaration
                  (ne "double")
                  [(ne "x")]
                  (Infix Multiply (Identifier (ne "x")) (Number 2))
              ]
      parseResult `shouldBe` Right expected
    it "parses nested assignment" $ do
      code <- readFixture "nested-assignment"
      let parseResult = parseExpressionFromString code
      let expected =
            Module
              [ TopLevelDeclaration
                  (ne "a")
                  []
                  (Assignment (ne "b") [] (Identifier (ne "c")))
              ]
      parseResult `shouldBe` Right expected

ne :: String -> NonEmptyString
ne = NonEmptyString . NE.fromList

readFixture :: String -> IO String
readFixture name = readFile ("test/fixtures/" ++ name ++ ".tree")
