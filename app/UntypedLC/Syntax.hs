module UntypedLC.Syntax where

import Data.Text

data Term n
  = Var n
  | Abs Text (Term n)
  | App (Term n) (Term n)
  deriving (Eq, Show)