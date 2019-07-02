{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Wasm
  ( Expression(..)
  , Module(..)
  , printWasm
  , forestModuleToWasm
  ) where

import qualified Language as F

import Control.Arrow ((***))
import Data.Char
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Generics.Deriving as G
import Text.RawString.QQ

import TypeChecker
import qualified TypeChecker as T

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
         Text

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

data Locals =
  Locals (Set F.Ident)

noLocals :: Locals
noLocals = Locals (Set.empty)

addLocal :: F.Ident -> Locals -> Locals
addLocal i (Locals l) = Locals (Set.insert i l)

addLocals :: [F.Ident] -> Locals -> Locals
addLocals is (Locals l) = Locals (Set.union l (Set.fromList is))

mergeLocals :: Locals -> Locals -> Locals
mergeLocals (Locals a) (Locals b) = Locals (Set.union a b)

showT :: Show a => a -> Text
showT = Text.pack . show

-- TODO - malloc could probably be better
prelude :: BytesAllocated -> Text
prelude bytesAllocated =
  let freeBlock =
        ("(global $freeblock (mut i32) (i32.const " <> showT bytesAllocated <>
         "))\n\n")
   in freeBlock <>
      [r|

(export "malloc" (func $malloc))
(func $malloc (param $size i32) (result i32)
  (local $address i32)
  (set_local $address (get_global $freeblock))
  (set_global $freeblock (i32.add (get_local $address) (get_local $size)))
  (return (get_local $address))
)

(func $string_copy (param $from i32) (param $to i32) (result i32)
  (local $index i32)
  (local $size i32)

  (set_local $index (i32.const 1))
  (set_local $size (i32.load8_u (get_local $from)))

  (loop $copy
    (i32.store8
      (i32.add (get_local $to) (get_local $index))
      (i32.load8_u (i32.add (get_local $from) (get_local $index)))
    )
    (set_local $index (i32.add (get_local $index) (i32.const 1)))
    (br_if $copy (i32.lt_s (get_local $index) (get_local $size)))
  )

  (return (get_local $size))
)

(func $string_add (param $a i32) (param $b i32) (result i32)
  (local $sum i32)
  (local $aSize i32)
  (local $newStr i32)
  (return
    (set_local $aSize (i32.load8_u (get_local $a)))
    (set_local $sum
      (i32.sub
        (i32.add
          (get_local $aSize)
          (i32.load8_u (get_local $b))
        )
        (i32.const 1)
      )
    )
    (set_local $newStr (call $malloc (i32.add (get_local $sum) (i32.const 1))))
    (i32.store8 (get_local $newStr) (get_local $sum))
    (call $string_copy (get_local $a) (get_local $newStr))
    (call $string_copy (get_local $b) (i32.sub (i32.add (get_local $newStr) (get_local $aSize)) (i32.const 1)))
    (get_local $newStr)
  )
)
|]

indent :: Int -> Text -> Text
indent level str =
  Text.intercalate "\n" $
  fmap (\line -> Text.replicate level " " <> line) (Text.lines str)

indent2 :: Text -> Text
indent2 = indent 2

forestModuleToWasm :: TypedModule -> Module
forestModuleToWasm (TypedModule topLevel) =
  foldl compileDeclaration initModule topLevel
  where
    initModule = Module [] 0

addTopLevel :: Module -> [TopLevel] -> Module
addTopLevel (Module topLevel bytes) newTopLevel =
  Module (topLevel <> newTopLevel) bytes

allocateBytes :: Module -> Int -> Module
allocateBytes (Module topLevel bytes) extraBytes =
  Module topLevel (bytes + extraBytes)

compileDeclaration :: Module -> TypedDeclaration -> Module
compileDeclaration m (TypedDeclaration name args _ fexpr) =
  let parameters = concatMap (fst <$> assignments) (fst <$> args)
      deconstruction = concatMap (snd <$> assignments) (fst <$> args)
      locals = Locals (Set.fromList parameters)
      (m', expr') = compileExpression m locals fexpr
      func =
        Func $
        Declaration
          name
          parameters
          (Sequence $ NE.fromList (deconstruction <> [expr']))
   in addTopLevel m' [func]

compileInlineDeclaration ::
     Module -> Locals -> TypedDeclaration -> (Maybe Expression, Module)
compileInlineDeclaration m (Locals l) (TypedDeclaration name args _ fexpr) =
  let parameters = concatMap (fst <$> assignments) (fst <$> args)
      locals = Locals (Set.union l (Set.fromList parameters))
      (m', expr') = compileExpression m locals fexpr
   in case args of
        [] -> (Just $ SetLocal name expr', m')
        _ ->
          (Nothing, addTopLevel m' [Func $ Declaration name parameters expr'])

compileExpressions ::
     Module -> NonEmpty TypedExpression -> (Module, [Expression])
compileExpressions m = foldl compile (m, [])
  where
    compile (m, xs) te =
      let (m', e) = compileExpression m (Locals Set.empty) te
       in (m', e : xs)

compileIdentifer ::
     Type -> F.Ident -> TypedDeclaration -> Set F.Ident -> Expression
compileIdentifer t i d l =
  case t of
    T.Applied (T.TL (T.TypeLambda _)) (T.Generic (F.Ident _)) ->
      if (Set.member i l)
        then GetLocal i
        else NamedCall i []
    T.TL (T.TypeLambda _) ->
      case d of
        T.TypedDeclaration _ [] _ (T.Number 0) -> GetLocal i
        T.TypedDeclaration _ [] _ (T.ADTConstruction _ []) -> NamedCall i []
        _ -> error (show d)
    _ -> GetLocal i

compileInfix ::
     Module
  -> Locals
  -> F.OperatorExpr
  -> TypedExpression
  -> TypedExpression
  -> (Module, Expression)
compileInfix m locals operator a b =
  let (m', aExpr) = compileExpression m locals a
      (m'', bExpr) = compileExpression m' locals b
      name = (F.Ident $ F.NonEmptyString 's' "tring_add")
   in case operator of
        F.StringAdd -> (m'', NamedCall name [aExpr, bExpr])
        _ -> (m'', Call (funcForOperator operator) [aExpr, bExpr])

compileApply ::
     Module
  -> Locals
  -> TypedExpression
  -> TypedExpression
  -> (Module, Expression)
compileApply m locals left right =
  case left of
    T.Apply _ (T.Identifier _ name _) r' ->
      let (m', exprs) = compileExpressions m [right, r']
       in (m', Sequence $ NE.fromList (exprs <> [NamedCall name []]))
    T.Identifier _ name _ ->
      let (m', r) = compileExpression m locals right
       in (m', NamedCall name [r])
    _ -> error $ "do not know what to do with " <> show left

compileLet ::
     Module
  -> Locals
  -> NonEmpty TypedDeclaration
  -> TypedExpression
  -> (Module, Expression)
compileLet m locals@(Locals l) declarations fexpr =
  let compileDeclaration' ::
           (Module, [Expression]) -> TypedDeclaration -> (Module, [Expression])
      compileDeclaration' (m', declarations) declaration =
        let (mExpr, m'') = compileInlineDeclaration m' locals declaration
         in case mExpr of
              Just expr -> (m'', declarations <> [expr])
              Nothing -> (m'', declarations)
      (m', declarationExpressions) =
        foldl compileDeclaration' (m, []) declarations
      names =
        NE.toList $ (\(TypedDeclaration name _ _ _) -> name) <$> declarations
      locals' = Locals $ Set.union l (Set.fromList names)
      (m'', expr') = compileExpression m' locals' fexpr
   in (m'', Sequence $ NE.fromList (declarationExpressions <> [expr']))

compileCase ::
     Module
  -> Locals
  -> TypedExpression
  -> NonEmpty (TypedArgument, TypedExpression)
  -> (Module, Expression)
compileCase m locals caseFexpr patterns =
  let (m', caseExpr) = compileCaseExpression m locals caseFexpr
      (m'', patternExprs) = patternsToWasm m' caseFexpr patterns
   in (m'', constructCase caseExpr patternExprs)
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
    patternsToWasm ::
         Module
      -> T.TypedExpression
      -> NE.NonEmpty (T.TypedArgument, T.TypedExpression)
      -> (Module, NonEmpty (Expression, Expression))
    patternsToWasm m caseFexpr patterns =
      let compilePattern ::
               (Module, [(Expression, Expression)])
            -> (T.TypedArgument, T.TypedExpression)
            -> (Module, [(Expression, Expression)])
          compilePattern (m', exprs) (a, b) =
            let (m'', aExpr, locals') = compileArgument m' caseFexpr a
                (m''', bExpr) =
                  compileExpression m'' (mergeLocals locals locals') b
             in (m''', exprs <> [(aExpr, bExpr)])
          (m', exprs) = foldl compilePattern (m, []) patterns
       in (m', NE.fromList exprs)

compileADTConstruction ::
     (Functor t, Foldable t) => Int -> t (F.Argument, b) -> Expression
compileADTConstruction tag args =
  Sequence
    (NE.fromList
       ([ SetLocal
            (ident "address")
            (NamedCall (ident "malloc") [Const $ (1 + length args) * 4])
        , Call (ident "i32.store") [GetLocal (ident "address"), Const tag]
        ] <>
        (store <$> zip [1 ..] (concatMap (fst <$> assignments) (fst <$> args))) <>
        [GetLocal (ident "address")]))
  where
    store :: (Int, F.Ident) -> Expression
    store (offset, i) =
      Call
        (ident "i32.store")
        [ Call
            (ident "i32.add")
            [GetLocal (ident "address"), Const (offset * 4)]
        , GetLocal i
        ]

compileString :: Module -> Text -> (Module, Expression)
compileString m str =
  let (Module _ address) = m
      m' = addTopLevel m [Data address str]
      m'' = allocateBytes m' (Text.length str + 1)
   in (m'', Const address)

compileExpression :: Module -> Locals -> TypedExpression -> (Module, Expression)
compileExpression m locals@(Locals l) fexpr =
  case fexpr of
    T.Identifier t i d -> (m, compileIdentifer t i d l)
    T.Number n -> (m, Const n)
    T.BetweenParens fexpr -> compileExpression m locals fexpr
    T.Infix _ operator a b -> compileInfix m locals operator a b
    T.Apply _ left right -> compileApply m locals left right
    T.Case _ caseFexpr patterns -> compileCase m locals caseFexpr patterns
    T.Let declarations fexpr -> compileLet m locals declarations fexpr
    T.String' str -> compileString m str
    T.ADTConstruction tag args -> (m, compileADTConstruction tag args)

assignments :: F.Argument -> ([F.Ident], [Expression])
assignments (F.AIdentifier i) = ([i], [])
assignments (F.ADeconstruction i args) =
  ([i], uncurry (compileDeconstructionAssignment i) <$> zip args [1 ..])
assignments (F.ANumberLiteral _) = ([], [])

compileDeconstructionAssignment :: F.Ident -> F.Argument -> Int -> Expression
compileDeconstructionAssignment i a n =
  case a of
    F.AIdentifier i' ->
      (SetLocal
         i'
         (Call
            (ident "i32.load")
            [Call (ident "i32.add") [GetLocal i, Const $ n * 4]]))
    _ -> Sequence []

compileCaseExpression ::
     Module -> Locals -> T.TypedExpression -> (Module, Expression)
compileCaseExpression m locals fexpr =
  case fexpr of
    Identifier (T.Applied _ _) i _ -> (m, Call (ident "i32.load") [GetLocal i])
    Identifier (T.TL (T.TypeLambda _)) i _ ->
      (m, Call (ident "i32.load") [GetLocal i])
    _ -> compileExpression m locals fexpr

compileArgument ::
     Module
  -> T.TypedExpression
  -> TypedArgument
  -> (Module, Expression, Locals)
compileArgument m caseFexpr arg =
  case arg of
    T.TAIdentifier _ i -> (m, GetLocal i, addLocal i noLocals)
    T.TANumberLiteral n -> (m, Const n, noLocals)
    T.TADeconstruction _ tag args ->
      let assignments = mapMaybe makeAssignment (zip args [1 ..])
          makeAssignment :: (T.TypedArgument, Int) -> Maybe Expression
          makeAssignment (arg, index) =
            case arg of
              TAIdentifier _ ident' ->
                Just
                  (SetLocal
                     ident'
                     (Call
                        (ident "i32.load")
                        [Call (ident "i32.add") [caseLocal, Const (index * 4)]]))
              _ -> Nothing
          localName (TAIdentifier _ ident') = Just ident'
          localName _ = Nothing
          locals = addLocals (mapMaybe localName args) noLocals
       in (m, Sequence (NE.fromList (assignments <> [Const tag])), locals)
  where
    caseLocal =
      case caseFexpr of
        T.Identifier _ name _ -> GetLocal name
        _ -> Const 0

eq32 :: F.Ident
eq32 = F.Ident $ F.NonEmptyString 'i' "32.eq"

funcForOperator :: F.OperatorExpr -> F.Ident
funcForOperator operator =
  F.Ident . uncurry F.NonEmptyString $
  case operator of
    F.Add -> ('i', "32.add")
    F.Subtract -> ('i', "32.sub")
    F.Multiply -> ('i', "32.mul")
    F.Divide -> ('i', "32.div_s")
    F.StringAdd -> ('s', "tring_add")

printWasm :: Module -> Text
printWasm (Module expressions bytesAllocated) =
  "(module\n" <> indent2 (prelude bytesAllocated) <> "\n\n" <>
  indent2 (printMemory bytesAllocated) <>
  "\n" <>
  indent2 (Text.intercalate "\n" $ printWasmTopLevel <$> expressions) <>
  "\n)"

printMemory :: BytesAllocated -> Text
printMemory bytes =
  case bytes of
    0 -> printMemory 1 -- TODO this is silly, we should omit the prelude instead
    _ ->
      "(memory $memory " <> showT pages <>
      ")\n(export \"memory\" (memory $memory))\n\n"
  where
    pageSize = 2 ** 16
    pages = ceiling $ fromIntegral bytes / pageSize

printWasmTopLevel :: TopLevel -> Text
printWasmTopLevel topLevel =
  case topLevel of
    Func (Declaration name args body) ->
      Text.unlines
        [ "(export \"" <> F.s name <> "\" (func $" <> F.s name <> "))"
        , printDeclaration (Declaration name args body)
        ]
    Data offset str ->
      "(data (i32.const " <> showT offset <> ") \"" <>
      escape (Text.length str + 1) <>
      str <>
      "\")"
  where
    escape n =
      case n of
        0 -> "\\" <> Text.singleton (chr 0)
        34 -> "\\\""
        _ -> Text.singleton (chr n)

printWasmExpr :: Expression -> Text
printWasmExpr expr =
  case expr of
    Sequence exprs ->
      "(block (result i32)\n" <> indent2 (Text.intercalate "\n" $ NE.toList (printWasmExpr <$> exprs)) <> "\n)"
    Const n -> "(i32.const " <> showT n <> ")"
    GetLocal name -> "(get_local $" <> F.s name <> ")"
    SetLocal name expr' ->
      "(set_local $" <> F.s name <> " " <> printWasmExpr expr' <> ")"
    Call name args ->
      "(" <> F.s name <> "\n" <>
      indent2 (Text.intercalate "\n" (printWasmExpr <$> args)) <>
      "\n)"
    NamedCall name args ->
      "(call $" <> F.s name <> "\n" <>
      indent2 (Text.intercalate "\n" (printWasmExpr <$> args)) <>
      "\n)"
    If conditional a b ->
      Text.unlines
        ([ "(if (result i32)"
         , indent2 $ printWasmExpr conditional
         , indent2 $ printWasmExpr a
         ] <>
         [indent2 $ maybe "(i32.const 0)" printWasmExpr b, ")"])

printDeclaration :: Declaration -> Text
printDeclaration (Declaration name args body) =
  Text.intercalate
    "\n"
    [ "(func $" <> F.s name <>
      Text.unwords (fmap (\x -> " (param $" <> x <> " i32)") (F.s <$> args)) <>
      " (result i32) " <>
      Text.unwords (printLocal <$> locals body)
    , indent2 $ Text.unlines ["(return", indent2 $ printWasmExpr body, ")"]
    , ")"
    ]
  where
    locals :: Expression -> [Text]
    locals expr' =
      case expr' of
        SetLocal name _ -> [F.s name]
        Sequence exprs -> concatMap locals $ NE.toList exprs
        If expr expr' mexpr ->
          locals expr <> locals expr' <> maybe [] locals mexpr
        Call _ exprs -> concatMap locals exprs
        NamedCall _ exprs -> concatMap locals exprs
        _ -> []
    -- TODO - there are probably other important cases we should handle here

printLocal :: Text -> Text
printLocal name = "(local $" <> name <> " i32)"

ident :: Text -> F.Ident
ident t = F.Ident $ F.NonEmptyString (Text.head t) (Text.tail t)
