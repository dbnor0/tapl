{-# LANGUAGE DerivingStrategies #-}

module TypedLC.Syntax where

import Data.Text
import Data.Map

data Type
  = BoolTy
  | NumTy
  | StringTy
  | AtomTy
  | UnitTy
  | FnTy Type Type
  | TupleTy [Type]
  | RecordTy (Map Text Type)
  | ListTy Type
  deriving stock (Eq, Show)

data Literal n
  = BoolL Bool
  | NumL Int
  | StringL String
  | TupleL [Term n]
  | RecordL (Map Text (Term n))
  | NilL Type
  | ConsL Type (Term n) (Term n)
  | UnitL
  deriving stock (Eq, Show)

data ListOp n
  = IsNil Type (Term n)
  | Head Type (Term n)
  | Tail Type (Term n)
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
  | ListT (ListOp n)
  | ProjectT (Term n) Text
  | IfT (Term n) (Term n) (Term n)
  | LetT Text (Term n) (Term n)
  | AsT (Term n) Type
  | AbsT Text Type (Term n)
  | AppT (Term n) (Term n)
  deriving stock (Eq, Show)


