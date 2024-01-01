{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}


module Effectful.Eval.SmallStep where

import Data.Text hiding (last, length, reverse, zip, empty)
import Effectful.Syntax qualified as S
import Control.Monad.Except (Except, MonadError (throwError), catchError, runExcept, runExceptT)
import Control.Monad.State
import Data.Map
import Prelude hiding (lookup)


type Location = Int
type Store = Map Location (S.Term Int)

type Exception = Text

type Eval m = (MonadState Store m, MonadError Exception m)

-- utils

showT :: Show a => a -> Text
showT = pack . show

-- environment

bindStore :: S.Term Int -> Store -> Store
bindStore t s = insert (size s) t s

-- evaluation

exec :: S.Term Int -> Either Exception (S.Term Int)
exec t = evalStateT (eval t) empty

trace :: S.Term Int -> Either Exception [S.Term Int]
trace t = evalStateT (trace' t) empty

eval :: Eval m => S.Term Int -> m (S.Term Int)
eval t = do
  r <- runExceptT (eval' t)
  case r of
    Left _ -> return t
    Right t' -> eval t'

trace' :: Eval m => S.Term Int -> m [S.Term Int]
trace' t = do
  r <- runExceptT (eval' t)
  case r of
    Left _ -> return [t]
    Right t' -> do
      ts <- trace' t'
      return $ t' : ts

eval' :: Eval m => S.Term Int -> m (S.Term Int)
eval' (S.IfT (S.LitT (S.BoolL True)) t1 _) = return t1
eval' (S.IfT (S.LitT (S.BoolL False)) _ t2) = return t2
eval' (S.IfT c t1 t2) = do
  c' <- eval' c
  return $ S.IfT c' t1 t2
eval' (S.AsT t _) = return t
eval' (S.AppT (S.AbsT x _ t) v) | isVal v = return $ substTerm v t
eval' (S.LetT x t1 t2) = do
  r <- runExceptT $ eval' t1
  case r of
    Left _ -> return $ substTerm t1 t2
    Right t1' -> return $ substTerm t1' t2
eval' (S.RefT v) | isVal v = do
  modify (bindStore v)
  s <- get
  return $ S.LitT $ S.Location $ size s - 1
eval' (S.RefT t) = do
  t' <- eval' t
  return $ S.RefT t'
eval' (S.DerefT (S.LitT (S.Location l))) = do
  s <- get
  case lookup l s of
    Nothing -> error $ "Invalid reference " <> show l <> " " <> show s
    Just t -> return t
eval' (S.DerefT t) = do
  t' <- eval' t
  return $ S.DerefT t'
eval' (S.AssignT (S.LitT (S.Location l)) v) | isVal v = do
  modify (insert l v)
  return $ S.LitT S.UnitL
eval' (S.AssignT t1 t2) = do
  r <- runExceptT $ eval' t1
  case r of
    Left _ -> do
      t2' <- eval' t2
      return $ S.AssignT t1 t2'
    Right t1' -> return $ S.AssignT t1' t2
eval' (S.ArithT op (S.LitT (S.NumL t1)) (S.LitT (S.NumL t2))) = do
  case op of
    S.Plus -> return $ S.LitT $ S.NumL $ t1 + t2
    S.Minus -> return $ S.LitT $ S.NumL $ t1 - t2
    S.Times -> return $ S.LitT $ S.NumL $ t1 * t2
    S.Divide -> return $ S.LitT $ S.NumL $ t1 `div` t2
eval' (S.ArithT op t1 t2) = do
  r <- runExceptT $ eval' t1
  case r of
    Left _ -> do
      t2' <- eval' t2
      return $ S.ArithT op t1 t2'
    Right t1' -> return $ S.ArithT op t1' t2
eval' (S.ProjectT (S.LitT (S.RecordL fs)) p) = do
  return $ fs ! p
eval' (S.ProjectT t p) = do
  t' <- eval' t
  return $ S.ProjectT t' p
eval' (S.AppT (S.LitT S.UnitL) t) = return t
eval' x@(S.AppT t1 t2) =
  if isVal t1 then do
    t2' <- eval' t2
    return $ S.AppT t1 t2'
  else do
    t1' <- eval' t1
    return $ S.AppT t1' t2
eval' _ = throwError "No rule applies"

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
        go c x (S.RefT t) = S.RefT (go c x t)
        go c x (S.DerefT t) = S.DerefT (go c x t)
        go c x (S.AssignT t1 t2) = S.AssignT (go c x t1) (go c x t2)
        go c x (S.VarT x') = if x' == x + c then shiftTerm s c else S.VarT x'
        go c x (S.AbsT x' ty t') = S.AbsT x' ty (go (c + 1) x t')
        go c x (S.AppT t1' t2') = S.AppT (go c x t1') (go c x t2')

        goLit c x (S.BoolL b) = S.BoolL b
        goLit c x (S.NumL n) = S.NumL n
        goLit c x (S.RecordL fs) = S.RecordL (fromList $ zip (keys fs) (go c x <$> elems fs))
        goLit c x S.UnitL = S.UnitL
        goLit c x (S.Location l) = S.Location l

-- shifting

shiftTerm :: S.Term Int -> Int -> S.Term Int
shiftTerm t d = go 0 t
  where go c (S.LitT lit) = S.LitT $ goLit c lit
        go c (S.VarT x) = if x >= c then S.VarT (x + d) else S.VarT x
        go c (S.ArithT op t1 t2) = S.ArithT op (go c t1)  (go c t2)
        go c (S.ProjectT t p) = S.ProjectT (go c t) p
        go c (S.IfT c' t1 t2) = S.IfT (go c c') (go c t1) (go c t2)
        go c (S.LetT x t1 t2) = S.LetT x (go c t1) (go (c + 1) t2)
        go c (S.RefT t) = S.RefT (go c t)
        go c (S.DerefT t) = S.DerefT (go c t)
        go c (S.AssignT t1 t2) = S.AssignT (go c t1) (go c t2)
        go c (S.AsT t ty) = S.AsT (go c t) ty
        go c (S.AbsT x ty t) = S.AbsT x ty (go (c + 1) t)
        go c (S.AppT t1 t2) = S.AppT (go c t1) (go c t2)

        goLit c (S.BoolL b) = S.BoolL b
        goLit c (S.NumL n) = S.NumL n
        goLit c (S.RecordL fs) = S.RecordL (fromList $ zip (keys fs) (go c <$> elems fs))
        goLit c S.UnitL = S.UnitL
        goLit c (S.Location l) = S.Location l

-- utils

isVal :: S.Term a -> Bool
isVal (S.AbsT {}) = True
isVal (S.LitT _) = True
isVal _ = False