{-# LANGUAGE DeriveGeneric #-}

module Language
  ( NonEmptyString(..)
  , OperatorExpr(..)
  , Ident(..)
  , Expression(..)
  , Declaration(..)
  , Annotation(..)
  , Module(..)
  , Constructor(..)
  , TopLevel(..)
  , ADT(..)
  , Comment
  , s
  , idToString
  , neToString
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Generics.Deriving as G

type Comment = String

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
      (NE.NonEmpty Constructor)
  deriving (Show, Eq, G.Generic)

data Declaration =
  Declaration (Maybe Comment)
              (Maybe Annotation)
              Ident
              [Ident]
              Expression
  deriving (Show, Eq, G.Generic)

data Annotation =
  Annotation Ident
             (NE.NonEmpty Ident)
  deriving (Show, Eq, G.Generic)

data Expression
  = Identifier Ident
  | Number Int
  | Infix OperatorExpr
          Expression
          Expression
  | Call Ident
         [Expression]
  | Case Expression
         (NE.NonEmpty (Expression, Expression))
  | Let (NE.NonEmpty Declaration)
        Expression
  | BetweenParens Expression
  | String' String
  | PrefixedComment Comment Expression
  deriving (Show, Eq, G.Generic)

data Constructor =
  Constructor Ident
              [Ident]
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
  deriving (Show, Eq)

newtype NonEmptyString =
  NonEmptyString (NE.NonEmpty Char)
  deriving (Show, Eq)

s :: Ident -> String
s = idToString

idToString :: Ident -> String
idToString (Ident str) = neToString str

neToString :: NonEmptyString -> String
neToString (NonEmptyString se) = NE.toList se
