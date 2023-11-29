module UntypedLC.Syntax where

import Data.Text

data Term i n
  = Var i n
  | Abs i Text (Term i n)
  | App i (Term i n) (Term i n)
  deriving Eq

instance (Show n) => Show (Term i n) where
  show (Var _ n) = show n
  show (Abs _ x t) = "(@" <> show x <> ". " <> show t <> ")"
  show (App _ t1 t2) = "(" <> show t1 <> " " <> show t2 <> ")"