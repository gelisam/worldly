{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, ScopedTypeVariables, ViewPatterns #-}
module Language.Wordly where

import Bound.Scope.Simple
import Bound.Class
import Bound.Var
import Control.Monad
import Data.Functor.Classes


data Type
  = UnitT          -- ^ unit type
  | Arr Type Type  -- ^ function type
  deriving (Eq,Show)

-- terms which can Synthesize their type
data STerm a
  = Var a
  | Ann (CTerm STerm a) Type       -- ^ type annotation
  | UnitV                          -- ^ the only value of type UnitT
  | App (STerm a) (CTerm STerm a)  -- ^ function application
  deriving (Eq,Show,Functor,Foldable,Traversable)

-- terms which are Checked against a type
data CTerm sTerm a
  = STerm (sTerm a)
  | Lam (Scope () sTerm a) -- ^ lambda abstraction
  deriving (Eq,Show,Functor,Foldable,Traversable)


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


instance Show1 STerm where
  liftShowsPrec sp sl = go
    where
      go d (Var v)   = showParen (d > app_prec)
                     $ showString "Var "
                     . sp (app_prec+1) v
      go d (Ann x t) = showParen (d > app_prec)
                     $ showString "Ann "
                     . liftShowsPrec sp sl (app_prec+1) x
                     . showString " "
                     . showsPrec (app_prec+1) t
      go _ UnitV     = showString "UnitV"
      go d (App x y) = showParen (d > app_prec)
                     $ showString "App "
                     . go (app_prec+1) x
                     . showString " "
                     . liftShowsPrec sp sl (app_prec+1) y
      
      app_prec = 10

instance Show1 sTerm => Show1 (CTerm sTerm) where
  liftShowsPrec sp sl = go
    where
      go d (STerm x) = showParen (d > app_prec)
                     $ showString "STerm "
                     . liftShowsPrec sp sl (app_prec+1) x
      go d (Lam x)   = showParen (d > app_prec)
                     $ showString "Lam "
                     . liftShowsPrec sp sl (app_prec+1) x
      
      app_prec = 10


bAbstract :: forall f a b c. Monad f
          => (a -> Maybe b)
          -> Scope c f a
          -> Scope c (Scope b f) a
bAbstract f = Scope . abstract f' . unscope
  where
    f' :: Var c a -> Maybe b
    f' (F a) = f a
    f' _     = Nothing

bInstantiate :: forall f a b c. Monad f
             => (b -> f a)
             -> Scope c (Scope b f) a
             -> Scope c f a
bInstantiate f = Scope . instantiate f' . unscope
  where
    f' :: b -> f (Var c a)
    f' = fmap F . f


cAbstract :: forall sTerm a b. Monad sTerm
          => (a -> Maybe b)
          -> CTerm sTerm a
          -> CTerm (Scope b sTerm) a
cAbstract f (STerm x) = STerm (abstract f x)
cAbstract f (Lam x)   = Lam (bAbstract f x)

cInstantiate :: forall sTerm a b. Monad sTerm
             => (b -> sTerm a)
             -> CTerm (Scope b sTerm) a
             -> CTerm sTerm a
cInstantiate f (STerm x) = STerm (instantiate f x)
cInstantiate f (Lam x)   = Lam (bInstantiate f x)


cAbstract1 :: (Monad sTerm, Eq a)
          => a
          -> CTerm sTerm a
          -> CTerm (Scope () sTerm) a
cAbstract1 a = cAbstract (\b -> if a == b then Just () else Nothing)

cInstantiate1 :: Monad sTerm
              => sTerm a
              -> CTerm (Scope () sTerm) a
              -> CTerm sTerm a
cInstantiate1 = cInstantiate . const
