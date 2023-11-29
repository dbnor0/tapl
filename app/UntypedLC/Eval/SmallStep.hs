{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module UntypedLC.Eval.SmallStep where

import UntypedLC.Syntax qualified as S
import Common.Parser

import Data.Text hiding (length, last, elem)
import Data.List
import Data.Set qualified as Set
import Control.Monad

type Term = S.Term ParseInfo
type FreeVar = Text
type BoundVar = Text

type Exception = Text

toNameless :: Term Text -> Term Int
toNameless t = removenames bound [] t
  where bound = Set.elems (getFree t [] Set.empty)

getFree :: Term Text -> [BoundVar] -> Set.Set FreeVar -> Set.Set FreeVar
getFree (S.Var _ x) bound free =
  if x `notElem` bound && x `notElem` free then
    Set.insert x free
  else
    Set.empty
getFree (S.Abs _ x t) bound free = getFree t (x : bound) free
getFree (S.App _ t1 t2) bound free =
  let free' = getFree t1 bound free
  in free' <> getFree t2 bound free'

removenames :: [Text] -> [Text] -> Term Text -> Term Int
removenames free bound (S.Var i x)     =
  case lastIndexOf x (free <> bound) of
    Just x' -> S.Var i $ length (free <> bound) - x' - 1
    Nothing -> error "Lookup failed"
removenames free bound (S.Abs i x t)   =
  S.Abs i "" (removenames free (bound <> [x])  t)
removenames free bound (S.App i t1 t2) =
  S.App i (removenames free bound t1) (removenames free bound t2)

lastIndexOf :: Eq a => a -> [a] -> Maybe Int
lastIndexOf x xs = case elemIndices x xs of
   [] -> Nothing
   indices -> Just $ last indices

shiftTerm :: Term Int -> Int -> Term Int
shiftTerm t d = go 0 t
  where go c (S.Var i x) =
          if x >= c then
            S.Var i (x + d)
          else
            S.Var i x
        go c (S.Abs i x t) = S.Abs i x (go (c + 1) t)
        go c (S.App i t1 t2) = S.App i (go c t1) (go c t2)

substTerm :: Term Int -> Term Int -> Term Int
substTerm s t = shiftTerm (subst 0 (shiftTerm s 1) t) (-1)
  where subst x s = go 0 x
        go c x (S.Var i x') = if x' == x + c then shiftTerm s c else S.Var i x'
        go c x (S.Abs i x' t') = S.Abs i x' (go (c + 1) x t')
        go c x (S.App i t1' t2') = S.App i (go c x t1') (go c x t2')

eval' :: Term Int -> Either Exception (Term Int)
eval' (S.App i (S.Abs i' x t) v@(S.Abs {})) = return $ substTerm v t
eval' (S.App i t1 t2) =
  if isVal t1 then do
    t2' <- eval' t2
    return $ S.App i t1 t2'
  else do
    t1' <- eval' t1
    return $ S.App i t1' t2
eval' _ = Left "No rule applies"

eval :: Term Int -> Term Int
eval t =
  case eval' t of
    Left _ -> t
    Right t' -> eval t'

isVal :: Term a -> Bool
isVal (S.Abs {}) = True
isVal _ = False