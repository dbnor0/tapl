module TypedArith.Typecheck where

import TypedArith.Syntax
import Control.Monad
import Control.Monad.Except

typecheck :: Term -> Either Exception Type
typecheck True' = Right BoolT
typecheck False' = Right BoolT
typecheck Zero = Right NatT
typecheck (Succ t) = do
  t' <- typecheck t
  unless (t' == NatT) 
    (throwError $ Exception "succ requires a Nat argument")
  return NatT
typecheck (Pred t) = do
  t' <- typecheck t
  unless (t' == NatT) 
    (throwError $ Exception "pred requires a Nat argument")
  return NatT
typecheck (IsZero t) = do
  t' <- typecheck t
  unless (t' == NatT) 
    (throwError $ Exception "iszero requires a Nat argument")
  return BoolT
typecheck (If t1 t2 t3) = do
  t1' <- typecheck t1
  t2' <- typecheck t2
  t3' <- typecheck t3
  unless (t1' == BoolT && t2' == t3') 
    (throwError $ Exception "if requires a Bool condition and branches of the same type")
  return t2'