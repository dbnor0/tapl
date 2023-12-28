{-# LANGUAGE DerivingStrategies #-}

module TypedLC.Syntax where

import Data.Text

data Type
  = BoolTy
  | NumTy
  | StringTy
  | AtomTy
  | UnitTy
  | FnTy Type Type
  | TupleTy [Type]
  deriving stock (Eq, Show)

data ArithOp
  = Plus
  | Minus
  | Times
  | Divide
  deriving stock (Eq, Show)

data Term n
  = BoolT Bool
  | NumT Int
  | StringT String
  | TupleT [Term n]
  | UnitT
  | IfT (Term n) (Term n) (Term n)
  | AsT (Term n) Type
  | LetT Text (Term n) (Term n)
  | ProjectT (Term n) (Term n)
  | ArithT ArithOp (Term n) (Term n)
  | VarT n
  | AbsT Text Type (Term n)
  | AppT (Term n) (Term n)
  deriving stock (Eq, Show)


