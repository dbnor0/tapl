{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module TypedLC.Eval.Nameless where

import Data.Text hiding (length, last, reverse, elem)
import Data.Set qualified as Set
import TypedLC.Syntax qualified as S
import Data.List
import Control.Monad.RWS (MonadReader (ask))
import Control.Monad.Reader
import Data.Foldable (elem)
import qualified GHC.Generics as Set


data GEnv f b = Env
  { free :: f Text
  , bound :: b Text
  }

type Env = GEnv [] []

type GConv m f b = MonadReader (GEnv f b) m
type Conv m = GConv m [] []

total :: Env -> [Text]
total (Env free bound) = free <> bound

bind :: Semigroup (b Text) => b Text -> GEnv f b -> GEnv f b
bind xs (Env free bound) = Env free (bound <> xs)

elem' :: (Foldable f, Foldable b) => Text -> GEnv f b -> Bool
elem' x (Env free bound) = x `elem` free || x `elem` bound

toNameless :: S.Term Text -> S.Term Int
toNameless t = runReader (removeNames t) (Env free [])
  where free = Set.elems (runReader (getFree t) (Env Set.empty []))

removeNames :: Conv m => S.Term Text -> m (S.Term Int)
removeNames (S.BoolT t) = return $ S.BoolT t
removeNames S.UnitT = return S.UnitT
removeNames (S.NumT n) = return $ S.NumT n
removeNames (S.TupleT ts) = do
  ts' <- traverse removeNames ts
  return $ S.TupleT ts'
removeNames (S.IfT c t1 t2) = do
  c' <- removeNames c
  t1' <- removeNames t1
  t2' <- removeNames t2
  return $ S.IfT c' t1' t2'
removeNames (S.AsT t ty) = do
  t' <- removeNames t
  return $ S.AsT t' ty
removeNames (S.LetT x t1 t2) = do
  local (bind [x]) $ do
    t1' <- removeNames t1
    t2' <- removeNames t2
    return $ S.LetT x t1' t2'
removeNames (S.ArithT op t1 t2) = do
  t1' <- removeNames t1
  t2' <- removeNames t2
  return $ S.ArithT op t1' t2'
removeNames (S.ProjectT t p) = do
  t' <- removeNames t
  p' <- removeNames p
  return $ S.ProjectT t' p'
removeNames (S.VarT x) = do
  env <- asks total
  case lastIndexOf x env of
    Just x' -> return $ S.VarT $ length env - x' - 1
    Nothing -> error $ "Lookup failed for " <> show x
removeNames (S.AbsT x ty t) = do
  local (bind [x]) $ do
    t' <- removeNames t
    return $ S.AbsT x ty t'
removeNames (S.AppT t1 t2) = do
  t1' <- removeNames t1
  t2' <- removeNames t2
  return $ S.AppT t1' t2'

lastIndexOf :: Eq a => a -> [a] -> Maybe Int
lastIndexOf x xs = case elemIndices x xs of
   [] -> Nothing
   indices -> Just $ last indices

getFree :: GConv m Set.Set [] => S.Term Text -> m (Set.Set Text)
getFree (S.BoolT _) = return Set.empty
getFree S.UnitT = return Set.empty
getFree (S.NumT _) = return Set.empty
getFree (S.TupleT ts) = do
  ts' <- traverse getFree ts
  return $ mconcat ts'
getFree (S.IfT c t1 t2) = do
  c' <- getFree c
  t1' <- getFree t1
  t2' <- getFree t2
  return $ c' <> t1' <> t2'
getFree (S.AsT t _) = getFree t
getFree (S.LetT x t1 t2) =
  local (bind [x]) $ do
    t1' <- getFree t1
    t2' <- getFree t2
    return $ t1' <> t2'
getFree (S.ArithT _ t1 t2) = do
  t1' <- getFree t1
  t2' <- getFree t2
  return $ t1' <> t2'
getFree (S.ProjectT t p) = do
  t' <- getFree t
  p' <- getFree p
  return $ t' <> p'
getFree (S.VarT x) = do
  env <- ask
  if not $ x `elem'` env then
    return $ Set.insert x (free env)
  else
    return Set.empty
getFree (S.AbsT x ty t) = do
  local (bind [x]) $ getFree t
getFree (S.AppT t1 t2) = do
  t1' <- getFree t1
  t2' <- getFree t2
  return $ t1' <> t2'

fromNameless :: Conv m => S.Term Int -> m (S.Term Text)
fromNameless (S.BoolT b) = return $ S.BoolT b
fromNameless S.UnitT = return S.UnitT
fromNameless (S.NumT n) = return $ S.NumT n
fromNameless (S.TupleT ts) = do
  ts' <- traverse fromNameless ts
  return $ S.TupleT ts'
fromNameless (S.IfT c t1 t2) = do
  c' <- fromNameless c
  t1' <- fromNameless t1
  t2' <- fromNameless t2
  return $ S.IfT c' t1' t2'
fromNameless (S.AsT t ty) = do
  t' <- fromNameless t
  return $ S.AsT t' ty
fromNameless (S.LetT x t1 t2) = do
  local (bind [x]) $ do
    t1' <- fromNameless t1
    t2' <- fromNameless t2
    return $ S.LetT x t1' t2'
fromNameless (S.ArithT op t1 t2) = do
  t1' <- fromNameless t1
  t2' <- fromNameless t2
  return $ S.ArithT op t1' t2'
fromNameless (S.ProjectT t p) = do
  t' <- fromNameless t
  p' <- fromNameless p
  return $ S.ProjectT t' p'
fromNameless (S.VarT x) = do
  env <- asks total
  return $ S.VarT $ reverse env !! x
fromNameless (S.AbsT x ty t) = do
  local (bind [x]) $ do
    t' <- fromNameless t
    return $ S.AbsT x ty t'
fromNameless (S.AppT t1 t2) = do
  t1' <- fromNameless t1
  t2' <- fromNameless t2
  return $ S.AppT t1' t2'



