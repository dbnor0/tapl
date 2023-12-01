module UntypedLC.Syntax where

import Data.Text

data Term n
  = Var n
  | Abs Text (Term n)
  | App (Term n) (Term n)
  deriving (Eq, Show)

-- instance (Show n) => Show (Term i n) where
--   show (Var _ n) = show n
--   show (Abs _ x t) = "(@" <> show x <> ". " <> show t <> ")"
--   show (App _ t1 t2) = "(" <> show t1 <> " " <> show t2 <> ")"