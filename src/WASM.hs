{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}

module WASM
  ( Expression(..)
  , Module(..)
  , printWasm
  , forestModuleToWasm
  ) where

import qualified HaskellSyntax as F

import Control.Arrow ((***))
import Data.Char
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Generics.Deriving as G

type BytesAllocated = Int

data Module =
  Module [TopLevel]
         BytesAllocated

data Declaration =
  Declaration F.Ident
              [F.Ident]
              Expression
  deriving (Show, Eq, G.Generic)

data TopLevel
  = Func Declaration
  | Data Int
         String

data Expression
  = Const Int
  | GetLocal F.Ident
  | SetLocal F.Ident
             Expression
  | Call F.Ident
         [Expression]
  | NamedCall F.Ident
              [Expression]
  | If Expression
       Expression
       (Maybe Expression)
  | Sequence (NE.NonEmpty Expression)
  deriving (Show, Eq)

indent :: Int -> String -> String
indent level str =
  intercalate "\n" $ map (\line -> replicate level ' ' ++ line) (lines str)

indent2 :: String -> String
indent2 = indent 2

forestModuleToWasm :: F.Module -> Module
forestModuleToWasm (F.Module declarations) =
  foldl compileDeclaration initModule declarations
  where
    initModule = Module [] 0

addTopLevel :: Module -> [TopLevel] -> Module
addTopLevel (Module topLevel bytes) newTopLevel =
  Module (topLevel ++ newTopLevel) bytes

allocateBytes :: Module -> Int -> Module
allocateBytes (Module topLevel bytes) extraBytes =
  Module topLevel (bytes + extraBytes)

compileDeclaration :: Module -> F.Declaration -> Module
compileDeclaration m (F.Declaration _ name args fexpr) =
  let (expr', m') = compileExpression m fexpr
      func = Func $ Declaration name args expr'
   in addTopLevel m' [func]

compileInlineDeclaration ::
     Module -> F.Declaration -> (Maybe Expression, Module)
compileInlineDeclaration m (F.Declaration _ name args fexpr) =
  let (expr', m') = compileExpression m fexpr
   in case args of
        [] -> (Just $ SetLocal name expr', m')
        _ -> (Nothing, addTopLevel m' [Func $ Declaration name args expr'])

compileExpression :: Module -> F.Expression -> (Expression, Module)
compileExpression m fexpr =
  case fexpr of
    F.Identifier i -> (GetLocal i, m)
    F.Number n -> (Const n, m)
    F.BetweenParens fexpr -> compileExpression m fexpr
    F.Infix operator a b ->
      let (aExpr, m') = compileExpression m a
          (bExpr, m'') = compileExpression m' b
       in (Call (funcForOperator operator) [aExpr, bExpr], m'')
    F.Call name arguments ->
      let compileArgument (m', exprs) fexpr =
            let (expr, m'') = compileExpression m' fexpr
             in (m'', exprs ++ [expr])
          (m', compiledArguments) = foldl compileArgument (m, []) arguments
       in (NamedCall name compiledArguments, m')
    F.Case caseFexpr patterns ->
      let (caseExpr, m') = compileExpression m caseFexpr
          (patternExprs, m'') = patternsToWasm m' patterns
       in (constructCase caseExpr patternExprs, m'')
    F.Let declarations fexpr ->
      let compileDeclaration' (m', declarations) declaration =
            let (mExpr, m'') = compileInlineDeclaration m' declaration
             in case mExpr of
                  Just expr -> (m'', declarations ++ [expr])
                  Nothing -> (m'', declarations)
          (m', declarationExpressions) =
            foldl compileDeclaration' (m, []) declarations
          (expr', m'') = compileExpression m' fexpr
       in (Sequence $ NE.fromList declarationExpressions <> [expr'], m'')
    F.String' str ->
      let (Module _ address) = m
          m' = addTopLevel m [Data address str]
          m'' = allocateBytes m' (length str + 1)
       in (Const address, m'')
  where
    constructCase ::
         Expression -> NE.NonEmpty (Expression, Expression) -> Expression
    constructCase caseExpr patterns =
      case patterns of
        [x] -> If (Call eq32 [caseExpr, fst x]) (snd x) Nothing
        (x :| xs) ->
          If
            (Call eq32 [caseExpr, fst x])
            (snd x)
            (Just (constructCase caseExpr (NE.fromList xs)))
    patternsToWasm m patterns =
      let compilePattern (m', exprs) (a, b) =
            let (aExpr, m'') = compileExpression m' a
                (bExpr, m''') = compileExpression m'' b
             in (m''', exprs ++ [(aExpr, bExpr)])
          (m', exprs) = foldl compilePattern (m, []) patterns
       in (NE.fromList exprs, m')

eq32 :: F.Ident
eq32 = F.Ident . F.NonEmptyString $ NE.fromList "i32.eq"

funcForOperator :: F.OperatorExpr -> F.Ident
funcForOperator operator =
  F.Ident . F.NonEmptyString $
  NE.fromList $
  case operator of
    F.Add -> "i32.add"
    F.Subtract -> "i32.sub"
    F.Multiply -> "i32.mul"
    F.Divide -> "i32.div_s"

printWasm :: Module -> String
printWasm (Module expressions bytesAllocated) =
  "(module\n" ++
  indent2 (printMemory bytesAllocated) ++ "\n" ++
  indent2 (intercalate "\n" $ printWasmTopLevel <$> expressions) ++ "\n)"

printMemory :: BytesAllocated -> String
printMemory bytes =
  case bytes of
    0 -> ""
    _ ->
      "(memory $memory " ++ show pages ++ ")\n(export \"memory\" (memory $memory))\n"
  where
    pageSize = 2 ** 16
    pages = ceiling $ (fromIntegral bytes) / pageSize

printWasmTopLevel :: TopLevel -> String
printWasmTopLevel topLevel =
  case topLevel of
    Func (Declaration name args body) ->
      unlines
        [ "(export \"" ++ F.s name ++ "\" (func $" ++ F.s name ++ "))"
        , printDeclaration (Declaration name args body)
        ]
    Data offset str ->
      "(data (i32.const " ++ show offset ++ ") \"" ++ escape (length str + 1) ++ str ++ "\")"
    where
      escape n =
        case n of
          0 ->  "\\" ++ [chr 0]
          34 -> "\\\""
          _ -> [chr n]

printWasmExpr :: Expression -> String
printWasmExpr expr =
  case expr of
    Sequence exprs -> intercalate "\n" $ NE.toList (printWasmExpr <$> exprs)
    Const n -> "(i32.const " ++ show n ++ ")"
    GetLocal name -> "(get_local $" ++ F.s name ++ ")"
    SetLocal name expr' ->
      "(set_local $" ++ F.s name ++ " " ++ printWasmExpr expr' ++ ")"
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

printDeclaration :: Declaration -> String
printDeclaration (Declaration name args body) =
  intercalate
    "\n"
    [ "(func $" ++
      F.s name ++
      unwords (map (\x -> " (param $" ++ x ++ " i32)") (F.s <$> args)) ++
      " (result i32) " ++ unwords (printLocal <$> locals body)
    , indent2 $ unlines ["(return", indent2 $ printWasmExpr body, ")"]
    , ")"
    ]
  where
    locals :: Expression -> [String]
    locals expr' =
      case expr' of
        SetLocal name _ -> [F.s name]
        Sequence exprs -> concatMap locals $ NE.toList exprs
        _ -> []
    -- TODO - there are probably other important cases we should handle here

printLocal :: String -> String
printLocal name = "(local $" ++ name ++ " i32)"
