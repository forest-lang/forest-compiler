{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.List.NonEmpty as NE
import System.Exit
import System.IO.Temp
import System.Process
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Compiler
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

instance Arbitrary Declaration where
  arbitrary = genDeclaration
  shrink = genericShrink

instance Arbitrary Ident where
  arbitrary = genIdent
  shrink (Ident s) = Ident <$> filter permittedWord (shrink s)

permittedWord :: NonEmptyString -> Bool
permittedWord (NonEmptyString s) = NE.toList s `notElem` rws

instance Arbitrary NonEmptyString where
  arbitrary = genString
  shrink (NonEmptyString s) =
    let charString = NE.toList s
        possibilities = shrink charString
        nonEmptyPossibilities = filter (not . null) possibilities
    in map (NonEmptyString . NE.fromList) nonEmptyPossibilities

genModule :: Gen Module
genModule = Module <$> listOf1 genDeclaration

genExpression :: Gen Expression
genExpression = oneof [genIdentifier, genNumber, genAssignment, genInfix]

genChar :: Gen Char
genChar = elements (['a' .. 'z'] ++ ['A' .. 'Z'])

genIdent :: Gen Ident
genIdent = Ident <$> suchThat genString permittedWord

genString :: Gen NonEmptyString
genString = NonEmptyString . NE.fromList <$> listOf1 genChar

genIdentifier :: Gen Expression
genIdentifier = do
  name <- genIdent
  return $ Identifier name

genNumber :: Gen Expression
genNumber = do
  number <- arbitrarySizedNatural
  return $ Number number

genAssignment :: Gen Expression
genAssignment = Assignment <$> genDeclaration

genDeclaration :: Gen Declaration
genDeclaration = do
  name <- genIdent
  args <- listOf genIdent
  expr <- genExpression
  return $ Declaration name args expr

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
              [ Declaration
                  (ne "double")
                  [ne "a"]
                  (Infix Multiply (Identifier (ne "a")) (Number 2))
              , Declaration
                  (ne "half")
                  [ne "a"]
                  (Infix Divide (Identifier (ne "a")) (Number 2))
              ]
      parseResult `shouldBe` Right expected
    it "parses an assignment with a case statement" $ do
      code <- readFixture "case-statement"
      let parseResult = parseExpressionFromString code
      let expected =
            Module
              [ Declaration
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
      let parseResult = parseExpressionFromString code
      let expected =
            Module
              [ Declaration
                  (ne "test")
                  [ne "n"]
                  (Case
                     (Identifier (ne "n"))
                     [ (Number 0, Number 1)
                     , (Number 1, Number 1)
                     , (Identifier (ne "n"), Identifier (ne "n"))
                     ])
              , Declaration
                  (ne "double")
                  [ne "x"]
                  (Infix Multiply (Identifier (ne "x")) (Number 2))
              ]
      parseResult `shouldBe` Right expected
    it "parses nested assignment" $ do
      code <- readFixture "nested-assignment"
      let parseResult = parseExpressionFromString code
      let expected =
            Module
              [ Declaration
                  (ne "a")
                  []
                  (Assignment $ Declaration (ne "b") [] (Identifier (ne "c")))
              ]
      parseResult `shouldBe` Right expected

ne :: String -> Ident
ne = Ident . NonEmptyString . NE.fromList

readFixture :: String -> IO String
readFixture name = readFile ("test/fixtures/" ++ name ++ ".tree")
