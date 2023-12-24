{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TypedLC.Typecheck where

import Data.Text hiding (zip)
import Data.Map
import TypedLC.Syntax qualified as S
import Control.Monad.Reader
import Control.Monad.Except
import Prelude hiding (lookup)


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
typecheck (S.IfT c t1 t2) = do
  c' <- typecheck c
  when (c' /= S.BoolTy)
    (throwError "if condition must have Bool type")
  t1' <- typecheck t1
  t2' <- typecheck t2
  when (t1' /= t2')
    (throwError $ "if branches must evaluate to the same type: " <> showT t1' <> " " <> showT t2')
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