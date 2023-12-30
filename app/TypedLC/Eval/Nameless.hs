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

-- utils

lastIndexOf :: Eq a => a -> [a] -> Maybe Int
lastIndexOf x xs = case elemIndices x xs of
  [] -> Nothing
  indices -> Just $ last indices

-- environment

total :: Env -> [Text]
total (Env free bound) = free <> bound

bind :: Semigroup (b Text) => b Text -> GEnv f b -> GEnv f b
bind xs (Env free bound) = Env free (bound <> xs)

elem' :: (Foldable f, Foldable b) => Text -> GEnv f b -> Bool
elem' x (Env free bound) = x `elem` free || x `elem` bound

-- conversion to nameless form

toNameless :: S.Term Text -> S.Term Int
toNameless t = runReader (removeNames t) (Env free [])
  where free = Set.elems (runReader (getFree t) (Env Set.empty []))

removeNames :: Conv m => S.Term Text -> m (S.Term Int)
removeNames (S.LitT lit) = do
  lit' <- removeNamesLit lit
  return $ S.LitT lit'
removeNames (S.VarT x) = do
  env <- asks total
  case lastIndexOf x env of
    Just x' -> return $ S.VarT $ length env - x' - 1
    Nothing -> error $ "Lookup failed for " <> show x
removeNames (S.ArithT op t1 t2) = do
  t1' <- removeNames t1
  t2' <- removeNames t2
  return $ S.ArithT op t1' t2'
removeNames (S.ListT op) = do
  op' <- removeNamesList op
  return $ S.ListT op'
removeNames (S.ProjectT t p) = do
  t' <- removeNames t
  return $ S.ProjectT t' p
removeNames (S.IfT c t1 t2) = do
  c' <- removeNames c
  t1' <- removeNames t1
  t2' <- removeNames t2
  return $ S.IfT c' t1' t2'
removeNames (S.LetT x t1 t2) = do
  local (bind [x]) $ do
    t1' <- removeNames t1
    t2' <- removeNames t2
    return $ S.LetT x t1' t2'
removeNames (S.AsT t ty) = do
  t' <- removeNames t
  return $ S.AsT t' ty
removeNames (S.AbsT x ty t) = do
  local (bind [x]) $ do
    t' <- removeNames t
    return $ S.AbsT x ty t'
removeNames (S.AppT t1 t2) = do
  t1' <- removeNames t1
  t2' <- removeNames t2
  return $ S.AppT t1' t2'

removeNamesLit :: Conv m => S.Literal Text -> m (S.Literal Int)
removeNamesLit (S.BoolL b) = return $ S.BoolL b
removeNamesLit (S.NumL n) = return $ S.NumL n
removeNamesLit (S.StringL s) = return $ S.StringL s
removeNamesLit (S.TupleL ts) = do
  ts' <- traverse removeNames ts
  return $ S.TupleL ts'
removeNamesLit (S.NilL ty) = return $ S.NilL ty
removeNamesLit (S.ConsL ty x xs) = do
  x' <- removeNames x
  xs' <- removeNames xs
  return $ S.ConsL ty x' xs'
removeNamesLit S.UnitL = return S.UnitL

removeNamesList :: Conv m => S.ListOp Text -> m (S.ListOp Int)
removeNamesList (S.IsNil ty xs) = do
  xs' <- removeNames xs
  return $ S.IsNil ty xs'
removeNamesList (S.Head ty xs) = do
  xs' <- removeNames xs
  return $ S.Head ty xs'
removeNamesList (S.Tail ty xs) = do
  xs' <- removeNames xs
  return $ S.Tail ty xs'

-- conversion from nameless form

fromNameless :: S.Term Int -> [Text] -> S.Term Text
fromNameless t free = runReader (assignNames t) (Env free [])

assignNames :: Conv m => S.Term Int -> m (S.Term Text)
assignNames (S.LitT lit) = do
  lit' <- assignNamesLit lit
  return $ S.LitT lit'
assignNames (S.IfT c t1 t2) = do
  c' <- assignNames c
  t1' <- assignNames t1
  t2' <- assignNames t2
  return $ S.IfT c' t1' t2'
assignNames (S.AsT t ty) = do
  t' <- assignNames t
  return $ S.AsT t' ty
assignNames (S.LetT x t1 t2) = do
  local (bind [x]) $ do
    t1' <- assignNames t1
    t2' <- assignNames t2
    return $ S.LetT x t1' t2'
assignNames (S.ArithT op t1 t2) = do
  t1' <- assignNames t1
  t2' <- assignNames t2
  return $ S.ArithT op t1' t2'
assignNames (S.ProjectT t p) = do
  t' <- assignNames t
  return $ S.ProjectT t' p
assignNames (S.ListT op) = do
  op' <- assignNamesList op
  return $ S.ListT op'
assignNames (S.VarT x) = do
  env <- asks total
  return $ S.VarT $ reverse env !! x
assignNames (S.AbsT x ty t) = do
  local (bind [x]) $ do
    t' <- assignNames t
    return $ S.AbsT x ty t'
assignNames (S.AppT t1 t2) = do
  t1' <- assignNames t1
  t2' <- assignNames t2
  return $ S.AppT t1' t2'

assignNamesLit :: Conv m => S.Literal Int -> m (S.Literal Text)
assignNamesLit (S.BoolL b) = return $ S.BoolL b
assignNamesLit (S.NumL n) = return $ S.NumL n
assignNamesLit (S.StringL s) = return $ S.StringL s
assignNamesLit (S.TupleL ts) = do
  ts' <- traverse assignNames ts
  return $ S.TupleL ts'
assignNamesLit (S.NilL ty) = return $ S.NilL ty
assignNamesLit (S.ConsL ty x xs) = do
  x' <- assignNames x
  xs' <- assignNames xs
  return $ S.ConsL ty x' xs'
assignNamesLit S.UnitL = return S.UnitL

assignNamesList :: Conv m => S.ListOp Int -> m (S.ListOp Text)
assignNamesList (S.IsNil ty xs) = do
  xs' <- assignNames xs
  return $ S.IsNil ty xs'
assignNamesList (S.Head ty xs) = do
  xs' <- assignNames xs
  return $ S.Head ty xs'
assignNamesList (S.Tail ty xs) = do
  xs' <- assignNames xs
  return $ S.Tail ty xs'

-- getting free variables from a term

freeVars :: S.Term Text -> [Text]
freeVars t = Set.elems $ runReader (getFree t) (Env Set.empty [])

getFree :: GConv m Set.Set [] => S.Term Text -> m (Set.Set Text)
getFree (S.LitT lit) = getFreeLit lit
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
getFree (S.ProjectT t p) = getFree t
getFree (S.ListT op) = getFreeList op
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

getFreeLit :: GConv m Set.Set [] => S.Literal Text -> m (Set.Set Text)
getFreeLit (S.BoolL b) = return Set.empty
getFreeLit (S.NumL n) = return Set.empty
getFreeLit (S.StringL s) = return Set.empty
getFreeLit (S.TupleL ts) = do
  ts' <- traverse getFree ts
  return $ mconcat ts'
getFreeLit (S.NilL ty) = return Set.empty
getFreeLit (S.ConsL ty x xs) = do
  x' <- getFree x
  xs' <- getFree xs
  return $ x' <> xs'
getFreeLit S.UnitL = return Set.empty

getFreeList :: GConv m Set.Set [] => S.ListOp Text -> m (Set.Set Text)
getFreeList (S.IsNil ty xs) = getFree xs
getFreeList (S.Head ty xs) = getFree xs
getFreeList (S.Tail ty xs) = getFree xs
