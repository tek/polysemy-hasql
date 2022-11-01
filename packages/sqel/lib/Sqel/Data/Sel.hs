module Sqel.Data.Sel where

import Sqel.SOP.Constraint (symbolText)

data Sel =
  SelSymbol Symbol
  |
  SelPath [Symbol]
  |
  SelAuto
  |
  SelUnused

type SelW :: Sel -> Type
data SelW sel where
  SelWSymbol :: KnownSymbol name => Proxy name -> SelW ('SelSymbol name)
  SelWPath :: SelW ('SelPath path)
  SelWAuto :: SelW 'SelAuto
  SelWUnused :: SelW 'SelUnused

type MkSel :: Sel -> Constraint
class MkSel sel where
  mkSel :: SelW sel

instance (
    KnownSymbol sel
  ) => MkSel ('SelSymbol sel) where
  mkSel = SelWSymbol Proxy

instance MkSel 'SelAuto where
  mkSel = SelWAuto

instance MkSel 'SelUnused where
  mkSel = SelWUnused

instance MkSel ('SelPath p) where
  mkSel = SelWPath

selText :: Text -> SelW sel -> Text
selText alt = \case
  SelWAuto -> alt
  SelWPath -> alt
  SelWUnused -> alt
  SelWSymbol (Proxy :: Proxy name) -> symbolText @name

-- TODO path: store witness
showSelW :: SelW s -> Text
showSelW = \case
  SelWAuto -> "<auto>"
  SelWPath -> "<path>"
  SelWUnused -> "<unused>"
  SelWSymbol (Proxy :: Proxy sel) -> symbolText @sel
