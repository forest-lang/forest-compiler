{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Test.Hspec
import Test.QuickCheck

import Lib

instance Arbitrary Expression where
  arbitrary = genExpression

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

propParseAndPrint :: Expression -> Bool
propParseAndPrint expr =
  let output = printExpression expr
      reparsedExpr = parseExpressionFromString output
  in case reparsedExpr of
       Right newExpr -> head newExpr == expr
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
    -- it "parses calls in cases correctly" $ do
    --   let expression = Case (Identifier "a") [(Number 0,Call "f" [Identifier "g"]),(Identifier "z",Identifier "a")]
    --   let printedCode = printExpression expression
    --   let reparsed = parseExpressionFromString printedCode
    --   reparsed `shouldBe` Right [expression]

readFixture :: String -> IO String
readFixture name = readFile ("test/fixtures/" ++ name ++ ".tree")
