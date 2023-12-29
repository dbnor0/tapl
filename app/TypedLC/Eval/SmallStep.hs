{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module TypedLC.Eval.SmallStep where

import Data.Text hiding (last, length, reverse)
import TypedLC.Syntax qualified as S
import Data.Map.Ordered
import Control.Monad.Except (Except, MonadError, catchError)
import Control.Monad.State

type Exception = Text

-- evaluation

eval :: S.Term Int -> S.Term Int
eval t =
  case eval' t of
    Left _ -> t
    Right t' -> eval t'

trace :: S.Term Int -> [S.Term Int]
trace t =
  case eval' t of
    Left _ -> [t]
    Right t' -> t' : trace t'

eval' :: S.Term Int -> Either Exception (S.Term Int)
eval' (S.IfT (S.LitT (S.BoolL True)) t1 _) = return t1
eval' (S.IfT (S.LitT (S.BoolL False)) _ t2) = return t2
eval' (S.IfT c t1 t2) = do
  c' <- eval' c
  return $ S.IfT c' t1 t2
eval' (S.AsT t _) = return t
eval' (S.AppT (S.AbsT x _ t) v) | isVal v = return $ substTerm t v
eval' (S.LetT x t1 t2) = do
  case eval' t1 of
    Left _ -> return $ substTerm t1 t2
    Right t1' -> return $ substTerm t1' t2
eval' (S.ArithT op (S.LitT (S.NumL t1)) (S.LitT (S.NumL t2))) = do
  case op of
    S.Plus -> return $ S.LitT $ S.NumL $ t1 + t2
    S.Minus -> return $ S.LitT $ S.NumL $ t1 - t2
    S.Times -> return $ S.LitT $ S.NumL $ t1 * t2
    S.Divide -> return $ S.LitT $ S.NumL $ t1 `div` t2
eval' (S.ArithT op t1 t2) = do
  case eval' t1 of
    Left _ -> do
      t2' <- eval' t2
      return $ S.ArithT op t1 t2'
    Right t1' -> return $ S.ArithT op t1' t2
eval' (S.ProjectT (S.LitT (S.TupleL ts)) (S.LitT (S.NumL p))) = do
  return $ ts !! p
eval' (S.ProjectT t (S.LitT (S.NumL p))) = do
  t' <- eval' t
  return $ S.ProjectT t' (S.LitT (S.NumL p))
eval' (S.LitT (S.ConsL ty x xs)) = do
  case eval' x of
    Left _ -> do
      xs' <- eval' xs
      return $ S.LitT $ S.ConsL ty x xs'
    Right x' -> return $ S.LitT $ S.ConsL ty x' xs
eval' (S.AppT (S.LitT S.UnitL) t) = return t
eval' x@(S.AppT t1 t2) =
  if isVal t1 then do
    t2' <- eval' t2
    return $ S.AppT t1 t2'
  else do
    t1' <- eval' t1
    return $ S.AppT t1' t2
eval' _ = Left "No rule applies"

evalList :: S.ListOp Int -> Either Exception (S.Term Int)
evalList (S.IsNil _ (S.LitT (S.NilL _))) = return $ S.LitT $ S.BoolL True
evalList (S.IsNil _ (S.LitT (S.ConsL {}))) = return $ S.LitT $ S.BoolL False
evalList (S.IsNil ty xs) = do
  xs' <- eval' xs
  return $ S.ListT $ S.IsNil ty xs'
evalList (S.Head _ (S.LitT (S.ConsL _ x _))) = return x
evalList (S.Head ty xs) = do
  xs' <- eval' xs
  return $ S.ListT $ S.Head ty xs'
evalList (S.Tail _ (S.LitT (S.ConsL _ _ xs))) = return xs
evalList (S.Tail ty xs) = do
  xs' <- eval' xs
  return $ S.ListT $ S.Tail ty xs'

-- substitution

substTerm :: S.Term Int -> S.Term Int -> S.Term Int
substTerm s t = shiftTerm (subst 0 (shiftTerm s 1) t) (-1)
  where subst x s = go 0 x
        go c x (S.LitT lit) = S.LitT $ goLit c x lit
        go c x (S.IfT c' t1 t2) = S.IfT (go c x c') (go c x t1) (go c x t2)
        go c x (S.AsT t ty) = S.AsT (go c x t) ty
        go c x (S.ArithT op t1 t2) = S.ArithT op (go c x t1)  (go c x t2)
        go c x (S.ProjectT t p) = S.ProjectT (go c x t) p
        go c x (S.LetT x' t1 t2) = S.LetT x' (go c x t1) (go (c + 1) x t2)
        go c x (S.VarT x') = if x' == x + c then shiftTerm s c else S.VarT x'
        go c x (S.AbsT x' ty t') = S.AbsT x' ty (go (c + 1) x t')
        go c x (S.AppT t1' t2') = S.AppT (go c x t1') (go c x t2')
        goLit c x (S.BoolL b) = S.BoolL b
        goLit c x (S.NumL n) = S.NumL n
        goLit c x (S.StringL s) = S.StringL s
        goLit c x (S.NilL ty) = S.NilL ty
        goLit c x (S.ConsL ty x' xs) = S.ConsL ty (go c x x') (go c x xs)
        goLit c x S.UnitL = S.UnitL
        goList c x (S.IsNil ty xs) = S.IsNil ty (go c x xs)
        goList c x (S.Head ty xs) = S.Head ty (go c x xs)
        goList c x (S.Tail ty xs) = S.Tail ty (go c x xs)

-- shifting

shiftTerm :: S.Term Int -> Int -> S.Term Int
shiftTerm t d = go 0 t
  where go c (S.LitT lit) = S.LitT $ goLit c lit
        go c (S.VarT x) = if x >= c then S.VarT (x + d) else S.VarT x
        go c (S.ArithT op t1 t2) = S.ArithT op (go c t1)  (go c t2)
        go c (S.ListT op) = S.ListT $ goList c op
        go c (S.ProjectT t p) = S.ProjectT (go c t) p
        go c (S.IfT c' t1 t2) = S.IfT (go c c') (go c t1) (go c t2)
        go c (S.LetT x t1 t2) = S.LetT x (go c t1) (go (c + 1) t2)
        go c (S.AsT t ty) = S.AsT (go c t) ty
        go c (S.AbsT x ty t) = S.AbsT x ty (go (c + 1) t)
        go c (S.AppT t1 t2) = S.AppT (go c t1) (go c t2)
        goLit c (S.BoolL b) = S.BoolL b
        goLit c (S.NumL n) = S.NumL n
        goLit c (S.StringL s) = S.StringL s
        goLit c (S.NilL ty) = S.NilL ty
        goLit c (S.ConsL ty x xs) = S.ConsL ty (go c x) (go c xs)
        goLit c S.UnitL = S.UnitL
        goList c (S.IsNil ty xs) = S.IsNil ty (go c xs)
        goList c (S.Head ty xs) = S.Head ty (go c xs)
        goList c (S.Tail ty xs) = S.Tail ty (go c xs)

-- utils

isVal :: S.Term a -> Bool
isVal (S.AbsT {}) = True
isVal (S.LitT _) = True
isVal _ = False