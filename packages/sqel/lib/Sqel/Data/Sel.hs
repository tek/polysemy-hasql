module Sqel.Data.Sel where

import Exon (exon)

import Sqel.SOP.Constraint (symbolText)

data Sel =
  SelSymbol Symbol
  |
  SelPath [Symbol]
  |
  SelAuto
  |
  SelUnused
  |
  SelIndex Symbol

type SelW :: Sel -> Type
data SelW sel where
  SelWSymbol :: KnownSymbol name => Proxy name -> SelW ('SelSymbol name)
  SelWPath :: SelW ('SelPath path)
  SelWAuto :: SelW 'SelAuto
  SelWUnused :: SelW 'SelUnused
  SelWIndex :: KnownSymbol name => Proxy name -> SelW ('SelIndex name)

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

-- TODO path: store witness
showSelW :: SelW s -> Text
showSelW = \case
  SelWAuto -> "<auto>"
  SelWPath -> "<path>"
  SelWUnused -> "<unused>"
  SelWSymbol (Proxy :: Proxy sel) -> symbolText @sel
  SelWIndex (Proxy :: Proxy sel) -> [exon|<index for #{symbolText @sel}>|]

type ReifySel :: Sel -> Constraint
class ReifySel sel where
  reifySel :: SelW sel -> Text

instance ReifySel ('SelSymbol sel) where
  reifySel (SelWSymbol Proxy) = symbolText @sel

instance ReifySel ('SelIndex sel) where
  reifySel (SelWIndex Proxy) = [exon|ph_sum_index__#{symbolText @sel}|]
