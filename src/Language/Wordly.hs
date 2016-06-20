module Language.Wordly where


data Type
  = UnitT          -- ^ unit type
  | Arr Type Type  -- ^ function type

-- terms which are Checked against a type
data CTerm a
  = STerm (STerm a)
  | Lam (STerm (Maybe a)) -- ^ lambda abstraction

-- terms which can Synthesize their type
data STerm a
  = Var a
  | Ann (CTerm a) Type       -- ^ type annotation
  | UnitV                    -- ^ the only value of type UnitT
  | App (STerm a) (CTerm a)  -- ^ function application
