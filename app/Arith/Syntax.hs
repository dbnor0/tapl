{-# LANGUAGE DerivingStrategies #-}

module Arith.Syntax where

data Exception = Exception

data Term a
  = True' a
  | False' a
  | If a (Term a) (Term a) (Term a)
  | Zero a
  | Succ a (Term a)
  | Pred a (Term a)
  | IsZero a (Term a)
  deriving stock (Eq)

instance Show (Term a) where
  show (True' _) = "true"
  show (False' _) = "false"
  show (If _ t1 t2 t3) 
    =  "if " <> show t1 
    <> " then " <> show t2 
    <> " else " <> show t3
  show (Zero _) = "0"
  show (Succ _ t) = "succ " <> show t
  show (Pred _ t) = "pred " <> show t
  show (IsZero _ t) = "iszero " <> show t

isNumericVal :: Term a -> Bool
isNumericVal (Zero _) = True
isNumericVal (Succ _ t) = isNumericVal t
isNumericVal (Pred _ t) = isNumericVal t
isNumericVal _ = False

isBooleanVal :: Term a -> Bool
isBooleanVal (True' _) = True
isBooleanVal (False' _) = True
isBooleanVal _ = False

isVal :: Term a -> Bool
isVal t = isNumericVal t || isBooleanVal t

