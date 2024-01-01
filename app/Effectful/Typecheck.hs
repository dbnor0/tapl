{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Effectful.Typecheck where

import Data.Text hiding (zip, length, elem, empty, length)
import Data.Map
import Effectful.Syntax qualified as S
import Control.Monad.Reader
import Control.Monad.Except
import Prelude hiding (lookup)
import Text.Read (readMaybe)
import Control.Monad.State

type Context = Map Int (Maybe S.Type)
type Store = Map Int S.Type

type Exception = Text

type TC m = (MonadReader Context m, MonadState Store m, MonadError Exception m)

-- utils

showT :: Show a => a -> Text
showT = pack . show

-- environment

mkCtx :: Int -> Context
mkCtx n = fromList $ fmap (, Nothing) [0..n - 1]

mkStore :: Store
mkStore = empty

shift :: Context -> Context
shift ctx = fromList [((+ 1) k, v) | (k, v) <- toList ctx]

bindCtx :: S.Type -> Context -> Context
bindCtx ty ctx = insert 0 (Just ty) (shift ctx)

bindStore :: S.Type -> Store -> Store
bindStore ty s = insert (size s) ty s

-- typechecking

tc :: S.Term Int -> [Text] -> Either Exception S.Type
tc t free = evalStateT (runReaderT (typecheck t) (mkCtx (length free))) mkStore

typecheck :: TC m => S.Term Int -> m S.Type
typecheck (S.LitT lit) = typecheckLit lit
typecheck (S.VarT x) = do
  ctx <- ask
  case lookup x ctx of
    Just (Just t) -> return t
    _ -> throwError $ "no binding found in context for " <> showT x <> " (" <> showT ctx <> ")"
typecheck (S.ArithT op t1 t2) = do
  t1' <- typecheck t1
  t2' <- typecheck t2
  when (t1' /= S.NumTy)
    (throwError $ showT op <> " requires its first operand to have Num type, but got " <> showT t1')
  when (t2' /= S.NumTy)
    (throwError $ showT op <> " requires its second operand to have Num type, but got " <> showT t2')
  return t2'
typecheck (S.ProjectT t p) = do
  t' <- typecheck t
  case (t', p') of
    (S.RecordTy fs, Nothing) -> do
      unless (p `elem` keys fs)
        (throwError $ "Invalid selector " <> p <> " for type " <> showT t')
      return $ fs ! p
    _ -> throwError "Invalid left operand for projection"
  where p' = readMaybe (unpack p) :: Maybe Int
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
  local (bindCtx ty) $ do
    typecheck t2
typecheck (S.RefT t) = do
  t' <- typecheck t
  modify (bindStore t')
  return $ S.RefTy t'
typecheck (S.DerefT t) = do
  t' <- typecheck t
  case t' of
    (S.RefTy ty) -> return ty
    _ -> throwError $ "cannot dereference " <> showT t'
typecheck (S.AssignT t1 t2) = do
  t1' <- typecheck t1
  t2' <- typecheck t2
  case t1' of
    (S.RefTy ty) -> do
      when (t2' /= ty) 
        (throwError $ "cannot assign type " <> showT t2' <> " to reference of type " <> showT ty)
      return S.UnitTy
    _ -> throwError $ "cannot assign to " <> showT t1'
typecheck (S.AsT t ty) = do
  t' <- typecheck t
  when (t' /= ty)
    (throwError $ "cannot ascribe " <> showT ty <> " to " <> showT t')
  return ty
typecheck (S.AbsT x ty t) = do
  local (bindCtx ty) $ do
    t' <- typecheck t
    return $ S.FnTy ty t'
typecheck t@(S.AppT t1 t2) = do
  t1' <- typecheck t1
  t2' <- typecheck t2
  case t1' of
    S.FnTy arg ret -> do
      when (t2' /= arg)
        (throwError $ "argument type mismatch, expected " <> showT arg <> ", got " <> showT t2' <> " in " <> showT t)
      return ret
    _ -> throwError $ "fn type expected, got " <> showT t1'

typecheckLit :: TC m => S.Literal Int -> m S.Type
typecheckLit (S.BoolL _) = return S.BoolTy
typecheckLit (S.NumL _) = return S.NumTy
typecheckLit (S.RecordL ts) = do
  tys <- traverse typecheck (elems ts)
  return $ S.RecordTy (fromList $ zip (keys ts) tys)
typecheckLit S.UnitL = return S.UnitTy
typecheckLit (S.Location l) = do
  store <- get
  case lookup l store of
    Just t -> return t
    _ -> throwError "no binding found in store"
