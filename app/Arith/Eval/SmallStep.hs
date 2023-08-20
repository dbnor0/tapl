{-# LANGUAGE LambdaCase #-}

module Arith.Eval.SmallStep where

import Arith.Syntax

import Control.Monad


eval1 :: Term a -> Either Exception (Term a)
eval1 = \case
  If _ (True' _) t1 _ -> return t1
  If _ (False' _) _ t2 -> return t2
  If i c t1 t2 -> do
    c' <- eval1 c
    return $ If i c' t1 t2
  Succ i t -> do
    t' <- eval1 t
    return $ Succ i t'
  Pred _ (Zero i) -> return $ Zero i
  Pred _ (Succ i t) -> do
    unless (isNumericVal t) 
      (Left Exception)
    return t
  Pred i t -> do
    t' <- eval1 t
    return $ Pred i t'
  IsZero _ (Zero i) -> return $ True' i
  IsZero _ (Succ i t) -> return $ False' i
  IsZero i t -> do
    t' <- eval1 t
    return $ IsZero i t'
  _ -> Left Exception

eval :: Term a -> Term a
eval t =
  case eval1 t of
    Left _ -> t
    Right t' -> eval t' 