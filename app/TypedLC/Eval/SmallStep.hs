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
eval' (S.IfT (S.BoolT True) t1 _) = return t1
eval' (S.IfT (S.BoolT False) _ t2) = return t2
eval' (S.IfT c t1 t2) = do
  c' <- eval' c
  return $ S.IfT c' t1 t2
eval' (S.AsT t _) = return t
eval' (S.AppT (S.AbsT x _ t) v@(S.AbsT {})) = return $ substTerm v t
eval' (S.AppT (S.AbsT x _ t) v@(S.BoolT _)) = return $ substTerm v t
eval' (S.AppT (S.AbsT x _ t) v@S.UnitT) = return $ substTerm v t
eval' (S.AppT (S.AbsT x _ t) v@(S.NumT _)) = return $ substTerm v t
eval' (S.AppT (S.AbsT x _ t) v@(S.TupleT _)) = return $ substTerm v t
eval' (S.LetT x t1 t2) = do
  case eval' t1 of
    Left _ -> return $ substTerm t1 t2
    Right t1' -> return $ substTerm t1' t2
eval' (S.ArithT op (S.NumT t1) (S.NumT t2)) = do
  case op of
    S.Plus -> return $ S.NumT $ t1 + t2
    S.Minus -> return $ S.NumT $ t1 - t2
    S.Times -> return $ S.NumT $ t1 * t2
    S.Divide -> return $ S.NumT $ t1 `div` t2
eval' (S.ArithT op t1 t2) = do
  case eval' t1 of
    Left _ -> do
      t2' <- eval' t2
      return $ S.ArithT op t1 t2'
    Right t1' -> return $ S.ArithT op t1' t2
eval' (S.ProjectT (S.TupleT ts) (S.NumT p)) = do
  return $ ts !! p
eval' (S.ProjectT t (S.NumT p)) = do
  t' <- eval' t
  return $ S.ProjectT t' (S.NumT p)
eval' (S.ConstT ty x xs) = do
  case eval' x of
    Left _ -> do
      xs' <- eval' xs
      return $ S.ConstT ty x xs'
    Right x' -> return $ S.ConstT ty x' xs
eval' (S.IsNilT _ (S.NilT _)) = return $ S.BoolT True
eval' (S.IsNilT _ (S.ConstT {})) = return $ S.BoolT False
eval' (S.IsNilT ty xs) = do
  xs' <- eval' xs
  return $ S.IsNilT ty xs'
eval' (S.HeadT _ (S.ConstT _ x _)) = return x
eval' (S.HeadT ty xs) = do
  xs' <- eval' xs
  return $ S.HeadT ty xs'
eval' (S.TailT _ (S.ConstT _ _ xs)) = return xs
eval' (S.TailT ty xs) = do
  xs' <- eval' xs
  return $ S.TailT ty xs'
eval' (S.AppT S.UnitT t) = return t
eval' x@(S.AppT t1 t2) =
  if isVal t1 then do
    t2' <- eval' t2
    return $ S.AppT t1 t2'
  else do
    t1' <- eval' t1
    return $ S.AppT t1' t2
eval' _ = Left "No rule applies"

substTerm :: S.Term Int -> S.Term Int -> S.Term Int
substTerm s t = shiftTerm (subst 0 (shiftTerm s 1) t) (-1)
  where subst x s = go 0 x
        go c x (S.BoolT t) = S.BoolT t
        go c x S.UnitT = S.UnitT
        go c x (S.NumT n) = S.NumT n
        go c x (S.TupleT ts) = S.TupleT ts
        go c x (S.NilT ty) = S.NilT ty
        go c x (S.ConstT ty x' xs) = S.ConstT ty (go c x x') (go c x xs)
        go c x (S.IsNilT ty xs) = S.IsNilT ty (go c x xs)
        go c x (S.HeadT ty xs) = S.HeadT ty (go c x xs)
        go c x (S.TailT ty xs) = S.TailT ty (go c x xs)
        go c x (S.IfT c' t1 t2) = S.IfT (go c x c') (go c x t1) (go c x t2)
        go c x (S.AsT t ty) = S.AsT (go c x t) ty
        go c x (S.ArithT op t1 t2) = S.ArithT op (go c x t1)  (go c x t2)
        go c x (S.ProjectT t p) = S.ProjectT (go c x t) p
        go c x (S.LetT x' t1 t2) = S.LetT x' (go c x t1) (go (c + 1) x t2)
        go c x (S.VarT x') = if x' == x + c then shiftTerm s c else S.VarT x'
        go c x (S.AbsT x' ty t') = S.AbsT x' ty (go (c + 1) x t')
        go c x (S.AppT t1' t2') = S.AppT (go c x t1') (go c x t2')

shiftTerm :: S.Term Int -> Int -> S.Term Int
shiftTerm t d = go 0 t
  where go c (S.BoolT t) = S.BoolT t
        go c S.UnitT = S.UnitT
        go c (S.NumT n) = S.NumT n
        go c (S.TupleT ts) = S.TupleT ts
        go c (S.NilT ty) = S.NilT ty
        go c (S.ConstT ty x xs) = S.ConstT ty (go c x) (go c xs)
        go c (S.IsNilT ty xs) = S.IsNilT ty (go c xs)
        go c (S.HeadT ty xs) = S.HeadT ty (go c xs)
        go c (S.TailT ty xs) = S.TailT ty (go c xs)
        go c (S.IfT c' t1 t2) = S.IfT (go c c') (go c t1) (go c t2)
        go c (S.AsT t ty) = S.AsT (go c t) ty
        go c (S.LetT x t1 t2) = S.LetT x (go c t1) (go (c + 1) t2)
        go c (S.ArithT op t1 t2) = S.ArithT op (go c t1)  (go c t2)
        go c (S.ProjectT t p) = S.ProjectT (go c t) p
        go c (S.VarT x) =
          if x >= c then
            S.VarT (x + d)
          else
            S.VarT x
        go c (S.AbsT x ty t) = S.AbsT x ty (go (c + 1) t)
        go c (S.AppT t1 t2) = S.AppT (go c t1) (go c t2)

isVal :: S.Term a -> Bool
isVal (S.AbsT {}) = True
isVal _ = False