{-# LANGUAGE ScopedTypeVariables #-}
module Language.Worldly.TypeChecker where

import Bound.Var
import Control.Monad

import Language.Worldly.Terms


(|>) :: (a -> Type) -> Type -> (Var () a -> Type)
(|>) _   t (B ()) = t
(|>) ctx _ (F v)  = ctx v


check :: forall a
       . (a -> Type)
      -> Type -> CTerm STerm a -> Maybe ()
check ctx t0 (STerm e) = do
    t <- synthesize ctx e
    guard (t0 == t)
check ctx t0 (Lam e) = do
    Arr s t <- return t0
    check (ctx |> s) t (cUnscope e)

synthesize :: (a -> Type)
           -> STerm a -> Maybe Type
synthesize ctx (Var v)   = return $ ctx v
synthesize ctx (Ann e t) = t <$ check ctx t e
synthesize _   UnitV     = return UnitT
synthesize ctx (App e1 e2) = do
    Arr s t <- synthesize ctx e1
    check ctx s e2
    return t
