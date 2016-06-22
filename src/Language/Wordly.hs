{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Language.Wordly where

import Bound
import Control.Monad
import Data.Functor.Classes


data Type
  = UnitT          -- ^ unit type
  | Arr Type Type  -- ^ function type
  deriving (Eq)

-- terms which can Synthesize their type
data STerm a
  = Var a
  | Ann (CTerm STerm a) Type       -- ^ type annotation
  | UnitV                          -- ^ the only value of type UnitT
  | App (STerm a) (CTerm STerm a)  -- ^ function application
  deriving (Eq,Functor,Foldable,Traversable)

-- terms which are Checked against a type
data CTerm sTerm a
  = STerm (sTerm a)
  | Lam (Scope () sTerm a) -- ^ lambda abstraction
  deriving (Eq,Functor,Foldable,Traversable)


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


instance Eq1 STerm where
  liftEq eq (Var v1)    (Var v2)    = eq v1 v2
  liftEq eq (Ann x1 t1) (Ann x2 t2) = liftEq eq x1 x2 && t1 == t2
  liftEq _  UnitV       UnitV       = True
  liftEq eq (App x1 y1) (App x2 y2) = liftEq eq x1 x2 && liftEq eq y1 y2
  liftEq _  _           _           = False

instance (Eq1 sTerm, Monad sTerm) => Eq1 (CTerm sTerm) where
  liftEq eq (STerm x1) (STerm x2) = liftEq eq x1 x2
  liftEq eq (Lam x1)   (Lam x2)   = liftEq eq x1 x2
  liftEq _  _          _          = False
