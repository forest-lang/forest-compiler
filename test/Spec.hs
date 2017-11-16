import Control.Monad
import Test.QuickCheck

import Lib

instance Arbitrary Expression where
  arbitrary = genExpression

genExpression :: Gen Expression
genExpression =
  oneof [genIdentifier, genNumber, genAssignment, genInfix, genCall, genCase]

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
  return $ Infix operator a b

genCall :: Gen Expression
genCall = do
  name <- genString
  args <- listOf1 genIdentifier
  return $ Call name args

(>*<) :: Gen a -> Gen b -> Gen (a,b)
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
main = quickCheck propParseAndPrint
