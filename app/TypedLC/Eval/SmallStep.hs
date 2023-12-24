{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module TypedLC.Eval.SmallStep where

import Data.Text hiding (last, length, reverse)
import TypedLC.Syntax qualified as S
import Data.Map.Ordered
import Control.Monad.Except (Except, MonadError)
import Control.Monad.State

type Exception = Text

eval :: S.Term Int -> S.Term Int
eval t =
  case eval' t of
    Left _ -> t
    Right t' -> eval t'

eval' :: S.Term Int -> Either Exception (S.Term Int)
eval' (S.IfT (S.BoolT True) t1 _) = return t1
eval' (S.IfT (S.BoolT False) _ t2) = return t2
eval' (S.IfT c t1 t2) = do
  c' <- eval' c
  return $ S.IfT c' t1 t2
eval' (S.AppT (S.AbsT x _ t) v@(S.AbsT {})) = return $ substTerm v t
eval' (S.AppT (S.AbsT x _ t) v@(S.BoolT _)) = return $ substTerm v t
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
        go c x (S.IfT c' t1 t2) = S.IfT (go c x c') (go c x t1) (go c x t2)
        go c x (S.VarT x') = if x' == x + c then shiftTerm s c else S.VarT x'
        go c x (S.AbsT x' ty t') = S.AbsT x' ty (go (c + 1) x t')
        go c x (S.AppT t1' t2') = S.AppT (go c x t1') (go c x t2')

shiftTerm :: S.Term Int -> Int -> S.Term Int
shiftTerm t d = go 0 t
  where go c (S.BoolT t) = S.BoolT t
        go c (S.IfT c' t1 t2) = S.IfT (go c c') (go c t1) (go c t2)
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