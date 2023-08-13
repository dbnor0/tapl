{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Exercise.UntypedArithmeticExpressions where
import Data.Foldable

data Term
  = True'
  | False'
  | Zero
  | Succ Term
  | Pred Term
  | IsZero Term
  | If Term Term Term
  deriving stock (Eq, Show)

consts :: Term -> [Term]
consts = \case
  Succ t -> consts t
  Pred t -> consts t
  IsZero t -> consts t
  If t1 t2 t3 -> consts t1 <> consts t2 <> consts t3
  t -> [t]

size :: Term -> Int
size = \case
  Succ t -> size t + 1
  Pred t -> size t + 1
  IsZero t -> size t
  If t1 t2 t3 -> size t1 + size t2 + size t3 + 1
  t -> 1

depth :: Term -> Int
depth = \case
  Succ t -> depth t + 1
  Pred t -> depth t + 1
  IsZero t -> depth t
  If t1 t2 t3 -> foldr' max 0 (depth <$> [t1, t2, t3]) + 1
  t -> 1