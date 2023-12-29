{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TypedLC.Typecheck where

import Data.Text hiding (zip, length)
import Data.Map
import TypedLC.Syntax qualified as S
import Control.Monad.Reader
import Control.Monad.Except
import Prelude hiding (lookup)
import Text.Read


type Env = Map Int (Maybe S.Type)
type Exception = Text

type TC m = (MonadReader Env m, MonadError Exception m)

-- utils

showT :: Show a => a -> Text
showT = pack . show

-- environment

mkEnv :: Int -> Env
mkEnv n = fromList $ fmap (, Nothing) [0..n - 1]

shift :: Env -> Env
shift env = fromList [((+ 1) k, v) | (k, v) <- toList env]

bind :: S.Type -> Env -> Env
bind ty e = insert 0 (Just ty) (shift e)

-- typechecking

tc :: S.Term Int -> [Text] -> Either Exception S.Type
tc t free = runReader (runExceptT (typecheck t)) (mkEnv (length free))

typecheck :: TC m => S.Term Int -> m S.Type
typecheck (S.LitT lit) = typecheckLit lit
typecheck (S.VarT x) = do
  env <- ask
  case lookup x env of
    Just (Just t) -> return t
    _ -> throwError "no binding found"
typecheck (S.ArithT op t1 t2) = do
  t1' <- typecheck t1
  t2' <- typecheck t2
  when (t1' /= S.NumTy)
    (throwError $ showT op <> " requires its first operand to have Num type")
  when (t2' /= S.NumTy)
    (throwError $ showT op <> " requires its second operand to have Num type")
  return t2'
typecheck (S.ListT op) = typecheckListOp op
typecheck (S.ProjectT t (S.LitT (S.NumL p))) = do
  t' <- typecheck t
  case t' of
    S.TupleTy tys -> 
      if p < 0 || p > length tys then
        throwError "Invalid left operand for projection"
      else 
        return $ tys !! p
    _ -> throwError "Invalid left operand for projection"
typecheck (S.IfT c t1 t2) = do
  c' <- typecheck c
  when (c' /= S.BoolTy)
    (throwError "if condition must have Bool type")
  t1' <- typecheck t1
  t2' <- typecheck t2
  when (t1' /= t2')
    (throwError $ "if branches must evaluate to the same type: " <> showT t1' <> " " <> showT t2')
  return t2'
typecheck (S.LetT x t1 t2) = do
  ty <- typecheck t1
  local (bind ty) $ do
    typecheck t2
typecheck (S.AsT t ty) = do
  t' <- typecheck t
  when (t' /= ty)
    (throwError $ "cannot ascribe " <> showT ty <> " to " <> showT t')
  return ty
typecheck (S.AbsT x ty t) = do
  local (bind ty) $ do
    t' <- typecheck t
    return $ S.FnTy ty t'
typecheck (S.AppT t1 t2) = do
  t1' <- typecheck t1
  t2' <- typecheck t2
  case t1' of
    S.FnTy arg ret -> do
      when (t2' /= arg)
        (throwError "argument type mismatch")
      return ret
    _ -> throwError $ "fn type expected, got " <> showT t1'

typecheckLit :: TC m => S.Literal Int -> m S.Type
typecheckLit (S.BoolL _) = return S.BoolTy
typecheckLit (S.NumL _) = return S.NumTy
typecheckLit (S.StringL _) = return S.StringTy
typecheckLit (S.TupleL ts) = do
  tys <- traverse typecheck ts
  return $ S.TupleTy tys
typecheckLit (S.NilL ty) = return $ S.ListTy ty
typecheckLit (S.ConsL ty x xs) = do
  x' <- typecheck x
  when (x' /= ty)
    (throwError $ "first argument of cons must have " <> showT ty <> " type")
  xs' <- typecheck xs
  when (xs' /= S.ListTy ty)
    (throwError $ "second argument of cons must have List " <> showT ty <> " type")
  return $ S.ListTy ty
typecheckLit S.UnitL = return S.UnitTy

typecheckListOp :: TC m => S.ListOp Int -> m S.Type
typecheckListOp (S.IsNil ty xs) = do
  xs' <- typecheck xs
  when (xs' /= S.ListTy ty)
    (throwError $ "argument of isnil must have List " <> showT ty <> " type")
  return S.BoolTy
typecheckListOp (S.Head ty xs) = do
  xs' <- typecheck xs
  when (xs' /= S.ListTy ty)
    (throwError $ "argument of head must have List " <> showT ty <> " type")
  return ty
typecheckListOp (S.Tail ty xs) = do
  xs' <- typecheck xs
  when (xs' /= S.ListTy ty)
    (throwError $ "argument of tail must have List " <> showT ty <> " type")
  return $ S.ListTy ty