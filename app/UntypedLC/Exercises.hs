{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}

module UntypedLC.Exercises where

import Data.Foldable (Foldable(foldr'))
import Prelude hiding (succ)

-- having these types universally quantified
-- would lead to some polytype errors
-- in the arithmetic functions I can't quite handle
type S = Int -> Int
type Z = Int
type Church = S -> Z -> Int

toChurch :: Int -> Church
toChurch 0 = \s z -> z
toChurch n = \s z -> (foldr' (.) id (replicate n s)) z

fromChurch :: Church -> Int
fromChurch n = n (+ 1) 0

zero :: Church
zero s z = z

one :: Church
one s z = s z

scc :: Church -> Church
scc n s z = s (n s z)

plus :: Church -> Church -> Church
plus m n s z = m s (n s z)

-- n s z applies s to z n times:
-- (\s z -> s .. s z) s z
-- s .. s z
--
-- if we replace the regular successor function in m
-- with the successor function repeated n times,
-- we should have m x n applications of s,
-- which indeed is the result of the arithmetic operation
-- we are trying to evaluate
--
-- n s is a function that will apply
-- s n times to its argument
--
-- if used in the context of another church numeral,
-- m (n s) is the function that applies
-- n successions of s, m times, to its argument
--
-- supplying z to this function yields the multiplication function
--
-- the book definition of times
-- times = λm. λn. m (plus n) c0
-- cannot be implemented with our
-- current representation of Church numerals
-- since we're using a concrete types for
-- the successor and zero functions, while
-- the untyped lambda calculus is.. untyped;
-- an implementation in a dynamically-typed langugage,
-- such as JavaScript would work as expected however:
--
-- const plus = m => n => s => z => m(s)(n(s)(z));
-- const times = m => n => m(plus(n))(zero);
times :: Church -> Church -> Church
times m n s = m (n s)