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

mkEnv :: Int -> Env
mkEnv n = fromList $ fmap (, Nothing) [0..n - 1]

shift :: Env -> Env
shift env = fromList [((+ 1) k, v) | (k, v) <- toList env]

bind :: S.Type -> Env -> Env
bind ty e = insert 0 (Just ty) (shift e)

showT :: Show a => a -> Text
showT = pack . show

typecheck :: TC m => S.Term Int -> m S.Type
typecheck (S.BoolT _) = return S.BoolTy
typecheck S.UnitT = return S.UnitTy
typecheck (S.NumT _) = return S.NumTy
typecheck (S.StringT _) = return S.StringTy
typecheck (S.TupleT ts) = do
  tys <- traverse typecheck ts
  return $ S.TupleTy tys
typecheck (S.IfT c t1 t2) = do
  c' <- typecheck c
  when (c' /= S.BoolTy)
    (throwError "if condition must have Bool type")
  t1' <- typecheck t1
  t2' <- typecheck t2
  when (t1' /= t2')
    (throwError $ "if branches must evaluate to the same type: " <> showT t1' <> " " <> showT t2')
  return t2'
typecheck (S.AsT t ty) = do
  t' <- typecheck t
  when (t' /= ty)
    (throwError $ "cannot ascribe " <> showT ty <> " to " <> showT t')
  return ty
typecheck (S.LetT x t1 t2) = do
  ty <- typecheck t1
  local (bind ty) $ do
    typecheck t2
typecheck (S.ProjectT t (S.NumT p)) = do
  t' <- typecheck t
  case t' of
    S.TupleTy tys -> 
      if p < 0 || p > length tys then
        throwError "Invalid left operand for projection"
      else 
        return $ tys !! p
    _ -> throwError "Invalid left operand for projection"
typecheck (S.ArithT op t1 t2) = do
  t1' <- typecheck t1
  t2' <- typecheck t2
  when (t1' /= S.NumTy)
    (throwError $ showT op <> " requires its first operand to have Num type")
  when (t2' /= S.NumTy)
    (throwError $ showT op <> " requires its second operand to have Num type")
  return t2'
typecheck (S.VarT x) = do
  env <- ask
  case lookup x env of
    Just (Just t) -> return t
    _ -> throwError "no binding found"
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