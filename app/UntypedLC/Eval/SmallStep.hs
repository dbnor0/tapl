{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module UntypedLC.Eval.SmallStep where

import UntypedLC.Syntax qualified as S
import Common.Parser

import Data.Text hiding (length, last, elem, reverse)
import Data.List
import Data.Set qualified as Set
import Control.Monad

type FreeVar = Text
type BoundVar = Text

type Exception = Text


-- evaluate a nameless term until a normal form is reached
eval :: S.Term Int -> S.Term Int
eval t =
  case eval' t of
    Left _ -> t
    Right t' -> eval t'

-- one step of call-by-value evaluation
-- only the outermost abstractions whose rhs has been
-- evaluated to a value are reduced
eval' :: S.Term Int -> Either Exception (S.Term Int)
eval' (S.App (S.Abs x t) v@(S.Abs {})) = return $ substTerm v t
eval' (S.App t1 t2) =
  if isVal t1 then do
    t2' <- eval' t2
    return $ S.App t1 t2'
  else do
    t1' <- eval' t1
    return $ S.App t1' t2
eval' _ = Left "No rule applies"

-- substitute occurences of 's' in the body 't'
-- of an abstracttion. we want to shift the free variables
-- of 's' so that they do not get captured by a potential
-- abstraction in 't'. after substitution is over,
-- we want to revert the shift in order to accomodate
-- for the bound variable we've substituted.
-- at the top level of 't', 0 refers to the bound variable
-- of the abstraction we're using up, however, every time we encounter
-- another inner abstraction, the index will increment, which 'c'
-- keeps track of. if we've found a variable occurance, we replace it
-- with 's', with its free variables shifted up by 'c', for each
-- abstraction travesed.
substTerm :: S.Term Int -> S.Term Int -> S.Term Int
substTerm s t = shiftTerm (subst 0 (shiftTerm s 1) t) (-1)
  where subst x s = go 0 x
        go c x (S.Var x') = if x' == x + c then shiftTerm s c else S.Var x'
        go c x (S.Abs x' t') = S.Abs x' (go (c + 1) x t')
        go c x (S.App t1' t2') = S.App (go c x t1') (go c x t2')

-- shift all free variables in a term 't'
-- by an increment 'd'. we identify free
-- variables by having an index greater than
-- the number of abstractions we've encountered so far
shiftTerm :: S.Term Int -> Int -> S.Term Int
shiftTerm t d = go 0 t
  where go c (S.Var x) =
          if x >= c then
            S.Var (x + d)
          else
            S.Var x
        go c (S.Abs x t) = S.Abs x (go (c + 1) t)
        go c (S.App t1 t2) = S.App (go c t1) (go c t2)

-- determine whether a term is a value (in normal form)
-- call-by-value only treats abstractions as values
isVal :: S.Term a -> Bool
isVal (S.Abs {}) = True
isVal _ = False

-- convert a regular term to a nameless one
toNameless :: S.Term Text -> S.Term Int
toNameless t = removenames bound [] t
  where bound = Set.elems (getFree t [] Set.empty)

fromNameless :: [FreeVar] -> [BoundVar] -> S.Term Int -> S.Term Text
fromNameless free bound (S.Var x)     = 
  S.Var $ reverse (free <> bound) !! x
fromNameless free bound (S.Abs x t)   =
  S.Abs x (fromNameless free (bound <> [x])  t)
fromNameless free bound (S.App t1 t2) =
  S.App (fromNameless free bound t1) (fromNameless free bound t2)

-- get the free variables of a term,
-- keeping track of them in a set
getFree :: S.Term Text -> [BoundVar] -> Set.Set FreeVar -> Set.Set FreeVar
getFree (S.Var x) bound free =
  if x `notElem` bound && x `notElem` free then
    Set.insert x free
  else
    Set.empty
getFree (S.Abs x t) bound free = getFree t (x : bound) free
getFree (S.App t1 t2) bound free =
  let free' = getFree t1 bound free
  in free' <> getFree t2 bound free'

-- convert a regular term to a nameless one,
-- while keeping track of the current
-- free & bound variables. whenever replacing
-- a variable with an index, we concatenate
-- the list of bound names, and perform a lookup
-- on the resulting list. the search function
-- always looks for the last index of an occurance
-- so that we always refer to the most recent addition
-- of a bound variable, in case there's shadowing.
-- free variables are always added first, and
-- bound once are appended in a stack-like manner.
-- this means that bound variables always have
-- priority over free ones, and the indices
-- of free variables will always get incremented
-- whenever we pass an abstraction, so that no clashing happens
removenames :: [FreeVar] -> [BoundVar] -> S.Term Text -> S.Term Int
removenames free bound (S.Var x)     =
  case lastIndexOf x (free <> bound) of
    Just x' -> S.Var $ length (free <> bound) - x' - 1
    Nothing -> error "Lookup failed"
removenames free bound (S.Abs x t)   =
  S.Abs x (removenames free (bound <> [x])  t)
removenames free bound (S.App t1 t2) =
  S.App (removenames free bound t1) (removenames free bound t2)

-- returns the index of the last appearance
-- of an element in a list
lastIndexOf :: Eq a => a -> [a] -> Maybe Int
lastIndexOf x xs = case elemIndices x xs of
   [] -> Nothing
   indices -> Just $ last indices