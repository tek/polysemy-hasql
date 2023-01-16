module Sqel.Data.Sel where

import Exon (exon)

import Sqel.SOP.Constraint (symbolText)

type IndexPrefix :: Maybe Symbol -> Symbol -> Constraint
class IndexPrefix spec prefix | spec -> prefix where

instance IndexPrefix 'Nothing "sqel_sum_index__" where

instance IndexPrefix ('Just prefix) prefix where

type IndexName :: Maybe Symbol -> Symbol -> Symbol -> Constraint
class KnownSymbol name => IndexName prefix tpe name | prefix tpe -> name where

instance (
    IndexPrefix prefixSpec prefix,
    name ~ AppendSymbol prefix tpe,
    KnownSymbol name
  ) => IndexName prefixSpec tpe name where

data Sel =
  SelSymbol Symbol
  |
  SelPath [Symbol]
  |
  SelAuto
  |
  SelUnused
  |
  SelIndex (Maybe Symbol) Symbol

type SelW :: Sel -> Type
data SelW sel where
  SelWSymbol :: KnownSymbol name => Proxy name -> SelW ('SelSymbol name)
  SelWPath :: SelW ('SelPath path)
  SelWAuto :: SelW 'SelAuto
  SelWUnused :: SelW 'SelUnused
  SelWIndex :: IndexName prefix tpe name => Proxy name -> SelW ('SelIndex prefix tpe)

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

type ReifySel :: Sel -> Symbol -> Constraint
class KnownSymbol name => ReifySel sel name | sel -> name where
  reifySel :: SelW sel -> Text

instance KnownSymbol name => ReifySel ('SelSymbol name) name where
  reifySel (SelWSymbol Proxy) = symbolText @name

instance (
    IndexPrefix prefixSpec prefix,
    name ~ AppendSymbol prefix sel,
    KnownSymbol name
  ) => ReifySel ('SelIndex prefixSpec sel) name where
  reifySel (SelWIndex Proxy) = symbolText @name
