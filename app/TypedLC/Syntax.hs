{-# LANGUAGE DerivingStrategies #-}

module TypedLC.Syntax where

import Data.Text

data Type
  = BoolTy
  | FnTy Type Type
  deriving stock (Eq, Show)

data Term n
  = BoolT Bool
  | IfT (Term n) (Term n) (Term n)
  | VarT n
  | AbsT Text Type (Term n)
  | AppT (Term n) (Term n)
  deriving stock (Eq, Show)


