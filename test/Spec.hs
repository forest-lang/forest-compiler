import Lib
import Test.QuickCheck

instance Arbitrary Expression where
  arbitrary = genExpression

genExpression :: Gen Expression
genExpression = oneof [genIdentifier, genNumber, genAssignment, genInfix]

genChar :: Gen Char
genChar = elements (['a'..'z'] ++ ['A'..'Z'])

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

genOperator :: Gen Operator
genOperator =
  elements [Add, Subtract, Multiply, Divide]

genInfix :: Gen Expression
genInfix = do
  operator <- genOperator
  a <- genNumber
  b <- genExpression

  return $ Infix operator a b

propParseAndPrint :: Expression -> Bool
propParseAndPrint expr =
  let
    output = printExpression expr
    reparsedExpr = parseExpressionFromString output
  in
    case reparsedExpr of
      Right newExpr -> newExpr == expr
      Left err -> False

main :: IO ()
main = quickCheck propParseAndPrint
