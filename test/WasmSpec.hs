{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module WasmSpec
  ( wasmSpecs
  ) where

import Control.Monad.Trans.State.Lazy
import Data.Either
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import qualified Data.Text as T
import System.Exit
import System.IO.Temp
import System.Process
import Test.Hspec
import Test.QuickCheck
import Text.RawString.QQ

import Compiler
import HaskellSyntax
import Language
import TypeChecker
import qualified TypeChecker as T
import Wasm
import qualified Wasm as W

import Arbitrary

instance Testable (IO Bool) where
  property = ioProperty

evalDeclarationCompileState :: DeclarationCompileState a -> (a, W.Module)
evalDeclarationCompileState a =
  runState (evalStateT a (W.UniqueLocals Map.empty)) (W.Module [] 0)

propCodeThatTypeChecksShouldCompile :: Language.Module -> IO Bool
propCodeThatTypeChecksShouldCompile m =
  case printWasm . forestModuleToWasm <$> checkModule m of
    Right wat -> do
      path <- writeSystemTempFile "wat" (unpack wat)
      exitCode <- system $ "wat2wasm " ++ show path ++ " -o /dev/null"
      case exitCode of
        ExitSuccess -> return True
        ExitFailure _ -> do
          _ <- system "mkdir -p failures"
          writeFile "./failures/last.tree" (unpack wat)
          return False
    Left _ -> return True

wasmSpecs :: SpecWith ()
wasmSpecs =
  parallel $
  describe "wasm code generation" $ do
    it "generates valid wasm for any well typed module" $ do
      withMaxSuccess
        1000
        (property (forAll genModule propCodeThatTypeChecksShouldCompile))
    it "correctly generates functions that return floats" $
      let typedModule =
            TypedModule
              [ TypedDeclaration
                  (Symbol 0 $ Ident (NonEmptyString 'g' "etX"))
                  [ TADeconstruction
                      (BindingSymbol . Symbol 99 $
                       Ident (NonEmptyString 'P' "layer"))
                      (ConstructorSymbol . Symbol 0 $
                       Ident (NonEmptyString 'P' "layer"))
                      0
                      [ TAIdentifier
                          Float'
                          (Symbol 1 $ Ident (NonEmptyString 'x' ""))
                      ]
                  ]
                  (Lambda
                     (TL (TypeLambda (Ident (NonEmptyString 'P' "layer"))))
                     Float')
                  (TypeChecker.Identifier
                     Float'
                     (Symbol 1 $ Ident (NonEmptyString 'x' "")))
              ]
       in forestModuleToWasm typedModule `shouldBe`
          Wasm.Module
            [ Func
                (Wasm.Declaration
                   (Ident (NonEmptyString 'g' "etX_0"))
                   [(Ident (NonEmptyString 'P' "layer_99"), I32)]
                   F32
                   (Sequence
                      (SetLocal
                         (Ident (NonEmptyString 'x' "_1"))
                         F32
                         (Call
                            (Ident (NonEmptyString 'f' "32.load"))
                            [ Call
                                (Ident (NonEmptyString 'i' "32.add"))
                                [ GetLocal
                                    (Ident (NonEmptyString 'P' "layer_99"))
                                , Const 4
                                ]
                            ]) :|
                       [GetLocal (Ident (NonEmptyString 'x' "_1"))])))
            ]
            0
    describe "assignment" $ do
      it "generates appropriate instructions for destructuring args" $
        let input =
              TADeconstruction
                (BindingSymbol . Symbol 3 $ ident "Player")
                (ConstructorSymbol . Symbol 0 $ ident "Player")
                0
                [TAIdentifier Num (Symbol 1 $ ident "x")]
            expectedInstructions =
              [ SetLocal
                  (ident "x_1")
                  I32
                  (Call
                     (ident "i32.load")
                     [ Call
                         (ident "i32.add")
                         [GetLocal (ident "Player_3"), Const 4]
                     ])
              ]
            instructions =
              evalState (assignments input) (UniqueLocals Map.empty)
         in instructions `shouldBe` expectedInstructions
      it "generates appropriate instructions for destructuring nested args" $
        let input =
              TADeconstruction
                (BindingSymbol . Symbol 99 $ ident "Player")
                (ConstructorSymbol . Symbol 0 $ ident "Player")
                0
                [ TADeconstruction
                    (BindingSymbol . Symbol 99 $ ident "Age")
                    (ConstructorSymbol . Symbol 1 $ ident "Age")
                    0
                    [TAIdentifier Num (Symbol 2 $ ident "age")]
                ]
            expectedInstructions =
              [ SetLocal
                  (ident "Age_99")
                  I32
                  (Call
                     (ident "i32.load")
                     [ Call
                         (ident "i32.add")
                         [GetLocal (ident "Player_99"), Const 4]
                     ])
              , SetLocal
                  (ident "age_2")
                  I32
                  (Call
                     (ident "i32.load")
                     [ Call
                         (ident "i32.add")
                         [GetLocal (ident "Age_99"), Const 4]
                     ])
              ]
            instructions =
              evalState (assignments input) (UniqueLocals Map.empty)
         in instructions `shouldBe` expectedInstructions
      it "generates unique names for locals" $
        let input =
              TADeconstruction
                (BindingSymbol . Symbol 99 $ ident "Player")
                (ConstructorSymbol . Symbol 0 $ ident "Player")
                0
                [ TADeconstruction
                    (BindingSymbol . Symbol 98 $ ident "Test")
                    (ConstructorSymbol . Symbol 1 $ ident "Test")
                    0
                    [TAIdentifier Num (Symbol 2 $ ident "a")]
                , TADeconstruction
                    (BindingSymbol . Symbol 97 $ ident "Test")
                    (ConstructorSymbol . Symbol 1 $ ident "Test")
                    0
                    [TAIdentifier Num (Symbol 4 $ident "a")]
                ]
            expectedInstructions =
              [ SetLocal
                  (ident "Test_98")
                  I32
                  (Call
                     (ident "i32.load")
                     [ Call
                         (ident "i32.add")
                         [GetLocal (ident "Player_99"), Const 4]
                     ])
              , SetLocal
                  (ident "a_2")
                  I32
                  (Call
                     (ident "i32.load")
                     [ Call
                         (ident "i32.add")
                         [GetLocal (ident "Test_98"), Const 4]
                     ])
              , SetLocal
                  (ident "Test_97")
                  I32
                  (Call
                     (ident "i32.load")
                     [ Call
                         (ident "i32.add")
                         [GetLocal (ident "Player_99"), Const 8]
                     ])
              , SetLocal
                  (ident "a_4")
                  I32
                  (Call
                     (ident "i32.load")
                     [ Call
                         (ident "i32.add")
                         [GetLocal (ident "Test_97"), Const 4]
                     ])
              ]
            instructions =
              evalState (assignments input) (UniqueLocals Map.empty)
         in instructions `shouldBe` expectedInstructions
      describe "compiling let declarations" $ do
        it "handles values" $
          let input :: T.TypedExpression
              input =
                T.Let
                  (NE.fromList
                     [ T.TypedDeclaration
                         (Symbol 0 (ident "a"))
                         []
                         Num
                         (T.Number 5)
                     ])
                  (T.Identifier Num (Symbol 0 (ident "a")))
              expectedExpr =
                Sequence
                  (NE.fromList
                     [ SetLocal (ident "a_0") I32 (Const 5)
                     , GetLocal (ident "a_0")
                     ])
              expectedModule = W.Module [] 0
              (expr, m) =
                evalDeclarationCompileState (compileExpression W.noLocals input)
           in do m `shouldBe` expectedModule
                 expr `shouldBe` expectedExpr
        it "handles functions" $
          let input :: T.TypedExpression
              input =
                T.Let
                  (NE.fromList
                     [ T.TypedDeclaration
                         (Symbol 0 (ident "a"))
                         [TAIdentifier Num (Symbol 1 (ident "i"))]
                         Num
                         (T.Number 5)
                     ])
                  (T.Apply
                     Num
                     (T.Identifier Num (Symbol 0 (ident "a")))
                     (T.Number 0))
              expectedExpr =
                W.Sequence (NE.fromList [NamedCall (ident "a_0") [Const 0]])
              expectedModule =
                W.Module
                  [ Func
                      (W.Declaration
                         (ident "a_0")
                         [(ident "i_1", I32)]
                         I32
                         (Const 5))
                  ]
                  0
              (expr, m) =
                evalDeclarationCompileState (compileExpression W.noLocals input)
           in do m `shouldBe` expectedModule
                 expr `shouldBe` expectedExpr
        it "handles closures" $
          let a = Symbol 0 (ident "a")
              f = Symbol 1 (ident "f")
              i = Symbol 2 (ident "i")
              input :: T.TypedExpression
              input =
                T.Let
                  (NE.fromList
                     [ T.TypedDeclaration a [] Num (T.Number 5)
                     , T.TypedDeclaration
                         f
                         [T.TAIdentifier Num i]
                         Num
                         (T.Infix
                            Num
                            Add
                            (T.Identifier Num i)
                            (T.Identifier Num a))
                     ])
                  (T.Apply Num (T.Identifier (Lambda Num Num) f) (T.Number 0))
              expectedExpr =
                W.Sequence
                  (NE.fromList
                     [ SetLocal (ident "a_0") I32 (Const 5)
                     , NamedCall (ident "f_1") [GetLocal (ident "a_0"), Const 0]
                     ])
              expectedModule =
                W.Module
                  [ Func
                      (W.Declaration
                         (ident "f_1")
                         [(ident "a_0", I32), (ident "i_2", I32)]
                         I32
                         (Call
                            (ident "i32.add")
                            [GetLocal (ident "i_2"), GetLocal (ident "a_0")]))
                  ]
                  0
              (expr, m) =
                evalDeclarationCompileState (compileExpression W.noLocals input)
           in do printWasm m `shouldBe` printWasm expectedModule
                 expr `shouldBe` expectedExpr
