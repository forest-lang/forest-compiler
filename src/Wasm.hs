{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module Wasm
  ( Expression(..)
  , Module(..)
  , WasmType(..)
  , Declaration(..)
  , TopLevel(..)
  , UniqueLocals(..)
  , printWasm
  , forestModuleToWasm
  , ident
  ) where

import qualified Language as F

import Control.Arrow ((***))
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Char
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.Ordered (OSet)
import qualified Data.Set.Ordered as OSet
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Generics.Deriving as G
import Text.RawString.QQ

import TypeChecker
import qualified TypeChecker as T

import qualified MemoryManagement as M

type BytesAllocated = Int

data Module =
  Module [TopLevel]
         BytesAllocated
  deriving (Show, Eq)

data WasmType
  = I32
  | F32
  deriving (Show, Eq, G.Generic)

data Declaration =
  Declaration F.Ident
              [(F.Ident, WasmType)]
              WasmType
              Expression
  deriving (Show, Eq, G.Generic)

data TopLevel
  = Func Declaration
  | Data Int
         Text
  deriving (Show, Eq)

data Expression
  = Const Int
  | FloatConst Float
  | GetLocal F.Ident
  | SetLocal F.Ident
             WasmType
             Expression
  | SetLocal' F.Ident
              WasmType
  | TeeLocal F.Ident
             WasmType
             Expression
  | Call F.Ident
         [Expression]
  | NamedCall F.Ident
              [Expression]
  | If Expression
       Expression
       (Maybe Expression)
  | If' Expression
        Expression
  | Block WasmType
          (NE.NonEmpty Expression)
  | Block' (NE.NonEmpty Expression)
  | Sequence (NE.NonEmpty Expression)
  | BR Int
  | Unreachable
  deriving (Show, Eq)

newtype Locals =
  Locals (Set F.Ident)

newtype UniqueLocals =
  UniqueLocals (Map F.Ident Int)

type CompileState = State Module

type DeclarationCompileState = StateT UniqueLocals CompileState

noLocals :: Locals
noLocals = Locals Set.empty

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

forestModuleToWasm :: M.Module -> Module
forestModuleToWasm (M.Module declarations) =
  execState (sequence $ compileDeclaration <$> declarations) initModule
  where
    initModule = Module [] 0

getAddress :: CompileState BytesAllocated
getAddress = gets bytesAllocated
  where
    bytesAllocated (Module _ bytes) = bytes

addTopLevel :: [TopLevel] -> CompileState ()
addTopLevel newTopLevel = modify transform
  where
    transform (Module topLevel bytes) = Module (newTopLevel <> topLevel) bytes

allocateBytes :: Int -> CompileState ()
allocateBytes i = modify transform
  where
    transform (Module topLevel bytes) = Module topLevel (bytes + i)

compileDeclaration :: M.Declaration -> CompileState ()
compileDeclaration (M.Declaration name closureArgs args fType statements) = do
  let expressions = compileStatements statements
  expr' <- evalStateT expressions (UniqueLocals Map.empty)
  let func =
        Func $
        Declaration
          (symbolToIdent name)
          parameters
          wasmType
          (Block wasmType $ NE.fromList (expr'))
  addTopLevel [func]
  where
    parameters = mArgType <$> (closureArgs <> args)
    wasmType = typeCast fType

typeCast :: M.Type -> WasmType
typeCast mType =
  case mType of
    M.Int -> I32
    M.Float -> F32

compileString :: Text -> DeclarationCompileState Expression
compileString str =
  lift $ do
    address <- getAddress
    addTopLevel [Data address str]
    allocateBytes (Text.length str + 1)
    return $ Const address

compileStatement :: M.Statement -> DeclarationCompileState [Expression]
compileStatement statement =
  case statement of
    M.LoadLocal symbol -> simply $ GetLocal (symbolToIdent symbol)
    M.PushLiteral (M.LitInt i) -> simply $ Const i
    M.PushLiteral (M.LitFloat f) -> simply $ FloatConst f
    M.PushLiteral (M.LitString s) -> only <$> compileString s
    M.StringAdd -> simply $ NamedCall (ident "string_add") []
    M.NumericOperation t operator ->
      simply $ Call (funcForOperator operator t) []
    M.Allocate bytes fields -> allocateRecord bytes fields
    M.LocalDeclaration declaration -> localDeclaration declaration
    M.CallDeclaration symbol -> simply $ NamedCall (symbolToIdent symbol) []
    M.Deconstruct deconstruction' ->
      return $ deconstruction Nothing deconstruction'
    M.Case t valueStatements patterns ->
      caseStatement t valueStatements patterns
  where
    only a = [a]
    simply a = return . only $ a

compileStatements ::
     Traversable t => t M.Statement -> StateT UniqueLocals CompileState [Expression]
compileStatements statements =
  concat <$> (sequence $ compileStatement <$> statements)

localDeclaration :: M.Declaration -> DeclarationCompileState [Expression]
localDeclaration d@(M.Declaration symbol _ args returnType statements) =
  case args of
    [] -> do
      statements <- compileStatements statements
      let wasmType = (typeCast returnType)
      return $ statements <> [SetLocal' (symbolToIdent symbol) wasmType]
    _ -> do
      _ <- lift $ compileDeclaration d
      return []

allocateRecord :: Int -> [M.SetField] -> DeclarationCompileState [Expression]
allocateRecord bytes fields =
  return $ (malloc bytes : storeFields) <> [GetLocal (ident "address")]
  where
    malloc bytes =
      SetLocal (ident "address") I32 (NamedCall (ident "malloc") [Const bytes])
    storeFields = storeField <$> fields
    storeField :: M.SetField -> Expression
    storeField (M.SetField offset fieldValue) =
      let (value, wType) =
            case fieldValue of
              M.FieldFromSymbol mType symbol ->
                (GetLocal (symbolToIdent symbol), typeCast mType)
              M.FieldFromInt int -> (Const int, I32)
       in Call
            (ident (printWasmType wType <> ".store"))
            [ Call (ident "i32.add") [GetLocal (ident "address"), Const offset]
            , value
            ]

deconstruction :: Maybe F.Ident -> M.Deconstruction -> [Expression]
deconstruction maybeIdent (M.Deconstruction localSymbol sourceSymbol _  offset mType) =
  [ GetLocal (fromMaybe (symbolToIdent sourceSymbol) maybeIdent)
  , Const offset
  , Call (ident "i32.add") []
  , Call (ident (printWasmType (typeCast mType) <> ".load")) []
  , SetLocal' (symbolToIdent localSymbol) (typeCast mType)
  ]

caseStatement ::
     M.Type
  -> NonEmpty M.Statement
  -> NonEmpty (M.Pattern, NonEmpty M.Statement)
  -> DeclarationCompileState [Expression]
caseStatement mType valueStatements patterns = do
  caseValues <- compileStatements valueStatements
  caseValueIdent <- getUniqueLocal (ident "case_result")
  let t = typeCast mType
  let setCaseValue =
        SetLocal caseValueIdent t (Block t (NE.fromList caseValues))
  patternExpressions <- sequence $ compilePattern caseValueIdent t <$> patterns
  return $
    [setCaseValue] <> (concat . NE.toList $ patternExpressions) <> [Unreachable]
  where
    compilePattern ::
         F.Ident
      -> WasmType
      -> (M.Pattern, NonEmpty M.Statement)
      -> DeclarationCompileState [Expression]
    compilePattern caseValueIdent t (pattern, statements) = do
      branchExpressions <- compileStatements statements
      case pattern of
        M.Wildcard s ->
          return $
          [ SetLocal (symbolToIdent s) t (GetLocal caseValueIdent)
          , (Block' $ (NE.fromList branchExpressions) <> [BR 2])
          ]
        M.NumericMatch n ->
          return $
          [ Const n
          , GetLocal caseValueIdent
          , If'
              (Call eq32 [])
              ((Block' $ (NE.fromList branchExpressions) <> [BR 3]))
          ]
        M.ExactMatch tag deconstructions -> do
          let deconstructionExpressions = concat $ deconstruction (Just caseValueIdent) <$> deconstructions
          return $
            [ GetLocal caseValueIdent
            , Call (ident "i32.load") []
            , Const tag
            , If'
                (Call eq32 [])
                ((Block' . NE.fromList $
                  deconstructionExpressions <> branchExpressions <> [BR 3]))
            ]

mArgType :: M.Arg -> (F.Ident, WasmType)
mArgType (M.Arg symbol mType) = (symbolToIdent symbol, typeCast mType)

getUniqueLocal :: Monad a => F.Ident -> StateT UniqueLocals a F.Ident
getUniqueLocal i = do
  count <- gets countForIdent
  modify updateCount
  return $ ident (F.s i <> "_" <> showT count)
  where
    updateCount (UniqueLocals map) = UniqueLocals (Map.insertWith (+) i 1 map)
    countForIdent (UniqueLocals map) = fromMaybe 0 (Map.lookup i map)

eq32 :: F.Ident
eq32 = F.Ident $ F.NonEmptyString 'i' "32.eq"

funcForOperator :: M.Operation -> M.Type -> F.Ident
funcForOperator operator t =
  let wasmType = printWasmType . typeCast $ t
      op =
        case (operator, t) of
          (M.Add, _) -> "add"
          (M.Subtract, _) -> "sub"
          (M.Multiply, _) -> "mul"
          (M.Divide, M.Float) -> "div"
          (M.Divide, _) -> "div_s"
   in ident (wasmType <> "." <> op)

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
    Func (Declaration name args wasmType body) ->
      Text.unlines
        [ "(export \"" <> F.s name <> "\" (func $" <> F.s name <> "))"
        , printDeclaration (Declaration name args wasmType body)
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
      Text.intercalate "\n" $ NE.toList (printWasmExpr <$> exprs)
    Block wasmType exprs ->
      "(block (result " <> printWasmType wasmType <> ")\n" <>
      indent2 (Text.intercalate "\n" $ NE.toList (printWasmExpr <$> exprs)) <>
      "\n)"
    Block' exprs ->
      "(block\n" <>
      indent2 (Text.intercalate "\n" $ NE.toList (printWasmExpr <$> exprs)) <>
      "\n)"
    Const n -> "(i32.const " <> showT n <> ")"
    FloatConst n -> "(f32.const " <> showT n <> ")"
    GetLocal name -> "(get_local $" <> F.s name <> ")"
    SetLocal name _ expr' ->
      "(set_local $" <> F.s name <> " " <> printWasmExpr expr' <> ")"
    SetLocal' name _ -> "(set_local $" <> F.s name <> ")"
    TeeLocal name _ expr' ->
      "(tee_local $" <> F.s name <> " " <> printWasmExpr expr' <> ")"
    Call name [] -> "(" <> F.s name <> ")"
    Call name args ->
      "(" <> F.s name <> "\n" <>
      indent2 (Text.intercalate "\n" (printWasmExpr <$> args)) <>
      "\n)"
    NamedCall name [] -> "(call $" <> F.s name <> ")"
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
    If' conditional a ->
      Text.unlines
        ([ "(if"
         , indent2 $ printWasmExpr conditional
         , indent2 $ printWasmExpr a
         , ")"
         ])
    BR n -> "(br " <> showT n <> ")"
    Unreachable -> "(unreachable)"

printWasmType :: WasmType -> Text
printWasmType wasmType =
  case wasmType of
    I32 -> "i32"
    F32 -> "f32"

printDeclaration :: Declaration -> Text
printDeclaration (Declaration name args wasmType body) =
  Text.intercalate
    "\n"
    [ "(func $" <> F.s name <> Text.unwords (printParam <$> args) <> " (result " <>
      printWasmType wasmType <>
      ") " <>
      Text.unwords (printLocal <$> locals body)
    , indent2 $ Text.unlines ["(return", indent2 $ printWasmExpr body, ")"]
    , ")"
    ]
  where
    printParam (name, wasmType) =
      " (param $" <> F.s name <> " " <> printWasmType wasmType <> ")"
    locals :: Expression -> [(Text, WasmType)]
    locals expr' = (\(a, b) -> (F.s a, b)) <$> findLocals expr'

findLocals :: Expression -> [(F.Ident, WasmType)]
findLocals expr' =
  case expr' of
    TeeLocal name wasmType expr -> [(name, wasmType)] <> findLocals expr
    SetLocal name wasmType expr -> [(name, wasmType)] <> findLocals expr
    SetLocal' name wasmType -> [(name, wasmType)]
    Sequence exprs -> concatMap findLocals $ NE.toList exprs
    Block _ exprs -> concatMap findLocals $ NE.toList exprs
    Block' exprs -> concatMap findLocals $ NE.toList exprs
    If expr expr' mexpr ->
      findLocals expr <> findLocals expr' <> maybe [] findLocals mexpr
    If' expr expr' -> findLocals expr <> findLocals expr'
    Call _ exprs -> concatMap findLocals exprs
    NamedCall _ exprs -> concatMap findLocals exprs
    Const _ -> []
    FloatConst _ -> []
    GetLocal _ -> []
    BR _ -> []
    Unreachable -> []

printLocal :: (Text, WasmType) -> Text
printLocal (name, wasmType) =
  "(local $" <> name <> " " <> printWasmType wasmType <> ")"

ident :: Text -> F.Ident
ident t = F.Ident $ F.NonEmptyString (Text.head t) (Text.tail t)

symbolToIdent :: T.Symbol -> F.Ident
symbolToIdent (Symbol n i) =
  if F.s i == "main"
    then ident "main"
    else ident (F.s i <> "_" <> showT n)

bindingToIdent :: T.Binding -> F.Ident
bindingToIdent (T.Binding _ symbol) = symbolToIdent symbol
