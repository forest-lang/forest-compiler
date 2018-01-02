{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Test.Hspec
import Test.QuickCheck

import Lib

instance Arbitrary Module where
  arbitrary = genModule

instance Arbitrary Expression where
  arbitrary = genExpression

genModule :: Gen Module
genModule = Module <$> listOf1 genAssignment

genExpression :: Gen Expression
genExpression = oneof [genIdentifier, genNumber, genAssignment, genInfix]

genChar :: Gen Char
genChar = elements (['a' .. 'z'] ++ ['A' .. 'Z'])

genString :: Gen String
genString = listOf1 genChar

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
       Left err -> False

main :: IO ()
main =
  hspec $ do
    describe "Forest haskell syntax" $ do
      it "prints and reparses arbitrary expressions losslessly" $
        property propParseAndPrint
      it "parses a module with multple assignments" $ do
        code <- readFixture "multiple-assignments"
        let parseResult = parseExpressionFromString code
        let expected =
              Module
                [ Assignment
                    "double"
                    ["a"]
                    (Infix Multiply (Identifier "a") (Number 2))
                , Assignment
                    "half"
                    ["a"]
                    (Infix Divide (Identifier "a") (Number 2))
                ]
        parseResult `shouldBe` Right expected
      it "parses an assignment with a case statement" $ do
        code <- readFixture "case-statement"
        let parseResult = parseExpressionFromString code
        let expected =
              Module
                [ Assignment
                    "test"
                    ["n"]
                    (Case
                       (Identifier "n")
                       [ (Number 0, Number 1)
                       , (Number 1, Number 1)
                       , (Identifier "n", Infix Add (Identifier "n") (Number 1))
                       ])
                ]
        parseResult `shouldBe` Right expected
      it
        "parses an assignment with a case statement followed by another assignment" $ do
        code <- readFixture "case-statement-and-more"
        let parseResult = parseExpressionFromString code
        let expected =
              Module
                [ Assignment
                    "test"
                    ["n"]
                    (Case
                       (Identifier "n")
                       [ (Number 0, Number 1)
                       , (Number 1, Number 1)
                       , (Identifier "n", Identifier "n")
                       ])
                , Assignment
                    "double"
                    ["x"]
                    (Infix Multiply (Identifier "x") (Number 2))
                ]
        parseResult `shouldBe` Right expected
      it "parses nested assignment" $ do
        code <- readFixture "nested-assignment"
        let parseResult = parseExpressionFromString code
        let expected =
              Module [Assignment "a" [] (Assignment "b" [] (Identifier "c"))]
        parseResult `shouldBe` Right expected

readFixture :: String -> IO String
readFixture name = readFile ("test/fixtures/" ++ name ++ ".tree")
