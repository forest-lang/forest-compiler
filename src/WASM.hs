{-# LANGUAGE OverloadedStrings #-}

module WASM
  ( Expression(..)
  , Module(..)
  , printWasm
  , forestExprToWasm
  , forestModuleToWasm
  ) where

import qualified Lib as F

import Control.Arrow ((***))
import Data.List (intercalate)
import Data.Maybe
import qualified Data.List.NonEmpty
import Data.Semigroup ((<>))
import Data.Text (Text)

newtype Module =
  Module [Expression]

data Expression
  = Const Int
  | Func F.NonEmptyString
         [F.NonEmptyString]
         Expression
  | GetLocal F.NonEmptyString
  | Call F.NonEmptyString
         [Expression]
  | NamedCall F.NonEmptyString
              [Expression]
  | If Expression
       Expression
       (Maybe Expression)

indent :: Int -> String -> String
indent level str =
  intercalate "\n" $ map (\line -> replicate level ' ' ++ line) (lines str)

indent2 :: String -> String
indent2 = indent 2

forestModuleToWasm :: F.Module -> Module
forestModuleToWasm (F.Module declarations) = Module (forestDeclarationToWasm <$> declarations)

forestDeclarationToWasm :: F.TopLevelDeclaration -> Expression
forestDeclarationToWasm (F.TopLevelDeclaration name args fexpr) = Func name args (forestExprToWasm fexpr)

forestExprToWasm :: F.Expression -> Expression
forestExprToWasm fexpr =
  case fexpr of
    F.Identifier i -> GetLocal i
    F.Number n -> Const n
    F.Assignment name args fexpr -> Func name args (forestExprToWasm fexpr)
    F.BetweenParens fexpr -> forestExprToWasm fexpr
    F.Infix operator a b ->
      Call (funcForOperator operator) [forestExprToWasm a, forestExprToWasm b]
    F.Call name arguments -> NamedCall name (map forestExprToWasm arguments)
    F.Case caseFexpr patterns ->
      constructCase (forestExprToWasm caseFexpr) (patternsToWasm patterns)
  where
    constructCase :: Expression -> [(Expression, Expression)] -> Expression
    constructCase caseExpr patterns =
      case patterns of
        [x] ->
          If (Call eq32 [caseExpr, fst x]) (snd (head patterns)) Nothing
        (x:xs) ->
          If
            (Call eq32 [caseExpr, fst x])
            (snd x)
            (Just (constructCase caseExpr xs))
        [] -> undefined -- TODO use nonempty to force this
    patternsToWasm = map (forestExprToWasm *** forestExprToWasm)

eq32 :: F.NonEmptyString
eq32 = F.NonEmptyString $ Data.List.NonEmpty.fromList "i32.eq"

funcForOperator :: F.OperatorExpr -> F.NonEmptyString
funcForOperator operator =
  F.NonEmptyString $ Data.List.NonEmpty.fromList $ case operator of
    F.Add -> "i32.add"
    F.Subtract -> "i32.sub"
    F.Multiply -> "i32.mul"
    F.Divide -> "i32.div_s"

printWasm :: Module -> String
printWasm (Module expressions) =
  "(module\n" ++
  indent2 (intercalate "\n" $ map printWasmExpr expressions) ++ "\n)"
  where
    printWasmExpr expr =
      case expr of
        Const n -> "(i32.const " ++ show n ++ ")"
        GetLocal name -> "(get_local $" ++ F.s name ++ ")"
        Call name args ->
          "(" ++
          F.s name ++ "\n" ++ indent2 (unlines (printWasmExpr <$> args)) ++ "\n)"
        NamedCall name args ->
          "(call $" ++
          F.s name ++ "\n" ++ indent2 (unlines (printWasmExpr <$> args)) ++ "\n)"
        If conditional a b ->
          unlines
            ([ "(if (result i32)"
             , indent2 $ printWasmExpr conditional
             , indent2 $ printWasmExpr a
             ] <>
             [indent2 $ maybe "(i32.const 0)" printWasmExpr b, ")"])
        Func name args body ->
          unlines
            [ "(export \"" ++ F.s name ++ "\" (func $" ++ F.s name ++ "))"
            , "(func $" ++
              F.s name ++
              unwords (map (\x -> " (param $" ++ x ++ " i32)") (F.s <$> args)) ++
              " (result i32)"
            , indent2 $ unlines ["(return", indent2 $ printWasmExpr body, ")"]
            , ")"
            ]
