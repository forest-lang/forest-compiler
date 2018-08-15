{-# LANGUAGE DeriveGeneric #-}

module Language
  ( NonEmptyString(..)
  , OperatorExpr(..)
  , Ident(..)
  , Expression(..)
  , Argument(..)
  , Declaration(..)
  , Annotation(..)
  , AnnotationType(..)
  , Module(..)
  , Constructor(..)
  , ConstructorType(..)
  , TopLevel(..)
  , ADT(..)
  , s
  , idToString
  , neToString
  ) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Generics.Deriving as G

newtype Module =
  Module [TopLevel]
  deriving (Show, Eq, G.Generic)

data TopLevel
  = Function Declaration
  | DataType ADT
  deriving (Show, Eq, G.Generic)

data ADT =
  ADT Ident
      [Ident]
      (NonEmpty Constructor)
  deriving (Show, Eq, G.Generic)

data Declaration =
  Declaration (Maybe Annotation)
              Ident
              [Ident]
              Expression
  deriving (Show, Eq, G.Generic)

data Annotation =
  Annotation Ident
             (NonEmpty AnnotationType)
  deriving (Show, Eq, G.Generic)

data AnnotationType
  = Concrete Ident
  | Parenthesized (NonEmpty AnnotationType)
  | TypeApplication AnnotationType
                    AnnotationType
  deriving (Show, Eq, G.Generic)

data Expression
  = Identifier Ident
  | Number Int
  | Infix OperatorExpr
          Expression
          Expression
  | Apply Expression
          Expression
  | Case Expression
         (NonEmpty (Argument, Expression))
  | Let (NonEmpty Declaration)
        Expression
  | BetweenParens Expression
  | String' Text
  deriving (Show, Eq, G.Generic)

data Argument
  = AIdentifier Ident
  | ADeconstruction Ident [Argument]
  | ANumberLiteral Int
  deriving (Show, Eq, G.Generic)

data Constructor =
  Constructor Ident
              (Maybe ConstructorType)
  deriving (Show, Eq, G.Generic)

data ConstructorType
  = CTConcrete Ident
  | CTApplied ConstructorType ConstructorType
  | CTParenthesized ConstructorType
  deriving (Show, Eq, G.Generic)

data OperatorExpr
  = Add
  | Subtract
  | Divide
  | Multiply
  | StringAdd
  deriving (Show, Eq, G.Generic)

newtype Ident =
  Ident NonEmptyString
  deriving (Show, Eq, Ord)

data NonEmptyString =
  NonEmptyString Char
                 Text
  deriving (Show, Eq, Ord)

s :: Ident -> Text
s = idToString

idToString :: Ident -> Text
idToString (Ident str) = neToString str

neToString :: NonEmptyString -> Text
neToString (NonEmptyString c t) = T.singleton c <> t
