{-# LANGUAGE DerivingStrategies #-}

module Effectful.Syntax where

import Data.Text
import Data.Map

data Type
  = BoolTy
  | NumTy
  | UnitTy
  | FnTy Type Type
  | RecordTy (Map Text Type)
  | RefTy Type
  deriving stock (Eq, Show)

data Literal n
  = BoolL Bool
  | NumL Int
  | RecordL (Map Text (Term n))
  | UnitL
  | Location Int
  deriving stock (Eq, Show)

data ArithOp
  = Plus
  | Minus
  | Times
  | Divide
  deriving stock (Eq, Show)

data Term n
  = LitT (Literal n)
  | VarT n
  | ArithT ArithOp (Term n) (Term n)
  | ProjectT (Term n) Text
  | IfT (Term n) (Term n) (Term n)
  | LetT Text (Term n) (Term n)
  | RefT (Term n)
  | DerefT (Term n)
  | AssignT (Term n) (Term n)
  | AsT (Term n) Type
  | AbsT Text Type (Term n)
  | AppT (Term n) (Term n)
  deriving stock (Eq, Show)

