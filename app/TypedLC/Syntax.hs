{-# LANGUAGE DerivingStrategies #-}

module TypedLC.Syntax where

import Data.Text

data Type
  = BoolTy
  | AtomTy
  | UnitTy
  | FnTy Type Type
  deriving stock (Eq, Show)

data Term n
  = BoolT Bool
  | UnitT
  | IfT (Term n) (Term n) (Term n)
  | AsT (Term n) Type
  | LetT Text (Term n) (Term n)
  | VarT n
  | AbsT Text Type (Term n)
  | AppT (Term n) (Term n)
  deriving stock (Eq, Show)


