{-# LANGUAGE DeriveFunctor #-}
module Language.Wordly where

import Bound
import Control.Monad


data Type
  = UnitT          -- ^ unit type
  | Arr Type Type  -- ^ function type

-- terms which can Synthesize their type
data STerm a
  = Var a
  | Ann (CTerm STerm a) Type       -- ^ type annotation
  | UnitV                          -- ^ the only value of type UnitT
  | App (STerm a) (CTerm STerm a)  -- ^ function application
  deriving (Functor)

-- terms which are Checked against a type
data CTerm sTerm a
  = STerm (sTerm a)
  | Lam (Scope () sTerm a) -- ^ lambda abstraction
  deriving (Functor)


instance Applicative STerm where
  pure = Var
  (<*>) = ap

instance Monad STerm where
  Var v   >>= f = f v
  Ann x t >>= f = Ann (x >>>= f) t
  UnitV   >>= _ = UnitV
  App x y >>= f = App (x >>= f) (y >>>= f)

instance Bound CTerm where
  STerm x >>>= f = STerm (x >>= f)
  Lam x   >>>= f = Lam (x >>>= f)
