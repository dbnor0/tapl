{-# LANGUAGE LambdaCase #-}

module TypedArith.Eval.SmallStep where

import TypedArith.Syntax

import Control.Monad

eval1 :: Term -> Either Exception Term
eval1 = \case
  If True' t1 _ -> return t1
  If False' _ t2 -> return t2
  If c t1 t2 -> do
    c' <- eval1 c
    return $ If c' t1 t2
  Succ t -> do
    t' <- eval1 t
    return $ Succ t'
  Pred Zero -> return Zero
  Pred (Succ t) -> return t
  Pred t -> do
    t' <- eval1 t
    return $ Pred t'
  IsZero Zero -> return True'
  IsZero (Succ t) -> return False'
  IsZero t -> do
    t' <- eval1 t
    return $ IsZero t'
  _ -> Left $ Exception "No evalation rule applies"

eval :: Term -> Term
eval t =
  case eval1 t of
    Left _ -> t
    Right t' -> eval t' 