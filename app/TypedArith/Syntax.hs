{-# LANGUAGE DerivingStrategies #-}

module TypedArith.Syntax where

newtype Exception = Exception String
  deriving stock (Eq, Show)

data Type
  = NatT
  | BoolT
  deriving stock (Eq, Show)

data Term
  = True'
  | False'
  | If Term Term Term
  | Zero
  | Succ Term
  | Pred Term
  | IsZero Term
  deriving stock (Eq)

instance Show Term where
  show True' = "true"
  show False' = "false"
  show (If t1 t2 t3) 
    =  "if " <> show t1 
    <> " then " <> show t2 
    <> " else " <> show t3
  show Zero = "0"
  show (Succ t) = "succ " <> show t
  show (Pred t) = "pred " <> show t
  show (IsZero t) = "iszero " <> show t

isNumericVal :: Term -> Bool
isNumericVal Zero = True
isNumericVal (Succ t) = isNumericVal t
isNumericVal (Pred t) = isNumericVal t
isNumericVal _ = False

isBooleanVal :: Term -> Bool
isBooleanVal True' = True
isBooleanVal False' = True
isBooleanVal _ = False

isVal :: Term -> Bool
isVal t = isNumericVal t || isBooleanVal t

