module Language.Wordly where

import Bound


data Type
  = UnitT          -- ^ unit type
  | Arr Type Type  -- ^ function type

-- terms which can Synthesize their type
data STerm a
  = Var a
  | Ann (CTerm STerm a) Type       -- ^ type annotation
  | UnitV                          -- ^ the only value of type UnitT
  | App (STerm a) (CTerm STerm a)  -- ^ function application

-- terms which are Checked against a type
data CTerm sTerm a
  = STerm (sTerm a)
  | Lam (Scope () sTerm a) -- ^ lambda abstraction
