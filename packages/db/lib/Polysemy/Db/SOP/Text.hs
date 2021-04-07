module Polysemy.Db.SOP.Text where

import Fcf (Eval, Exp)
import qualified Fcf.Data.Text as Fcf
import Fcf.Data.Text (ToSymbol, Uncons)

type family WithoutUnderscore (und :: Symbol) (cons :: Maybe (Symbol, Fcf.Text)) (name :: Symbol) :: Symbol where
  WithoutUnderscore und ('Just '(und, tail)) _ = Eval (ToSymbol tail)
  WithoutUnderscore _ _ name = name

data CanonicalName :: Symbol -> Exp Symbol

type instance (Eval (CanonicalName name)) =
  WithoutUnderscore "_" (Eval (Uncons ('Fcf.Text name))) name
