module NamelessRep.Exercises where

import Data.Map as M
import Data.List
import Data.Maybe
import Data.Bifunctor


data Term
  = Var String
  | Abs String Term
  | App Term Term
  deriving (Eq, Show)

-- character set chosen for name generation
alphabet :: [Char]
alphabet = ['a'..'z']

-- transform a given term into a nameless one
-- keeping track of free & bound variables
-- we're keeping all bound variables currently in scope in 'bound'
-- we're prepending the list of free variables to that list whenever looking symbols up
-- this way bound (& innermost) symbols have priority over free ones
-- note: representation of the identifiers can
--       probably be made more efficient
removenames :: [String] -> [String] -> Term -> Term
removenames free bound (Var x)     =
  case lastIndexOf x (free <> bound) of
    Just x' -> Var $ show $ length (free <> bound) - x' - 1
    Nothing -> error "Lookup failed"
removenames free bound (Abs x t)   =
  Abs "" (removenames free (bound <> [x])  t)
removenames free bound (App t1 t2) =
  App (removenames free bound t1) (removenames free bound t2)

lastIndexOf :: Eq a => a -> [a] -> Maybe Int
lastIndexOf x xs = case elemIndices x xs of
   [] -> Nothing
   indices -> Just $ last indices

-- the reverse operation of generating nameless terms
-- we keep track of free & bound variables like before
-- whenever we encounter an abstraction, we generate a new name
-- based on the size of the bound variable context
restorenames :: [String] -> [String] -> Term -> Term
restorenames free bound (Var x) = Var $ reverse (free <> bound) !! read x
restorenames free bound (Abs _ t) = Abs name (restorenames free (bound <> [name]) t)
  where name = getName $ length bound + 1
restorenames free bound (App t1 t2) =
  App (restorenames free bound t1) (restorenames free bound t2)

getName :: Int -> String
getName 0 = []
getName n 
  =  getName ((n - 1) `div` length alphabet) 
  <> [alphabet !! ((n - 1) `mod` length alphabet)]