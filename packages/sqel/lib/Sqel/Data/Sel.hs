module Sqel.Data.Sel where

import Exon (exon)

import Sqel.SOP.Constraint (symbolText)

data SelPrefix =
  DefaultPrefix
  |
  NoPrefix
  |
  SelPrefix Symbol

type family IndexPrefixed (spec :: SelPrefix) (name :: Symbol) :: Symbol where
  IndexPrefixed 'DefaultPrefix name = AppendSymbol "sqel_sum_index__" name
  IndexPrefixed 'NoPrefix name = name
  IndexPrefixed ('SelPrefix spec) name = AppendSymbol spec name

type IndexName :: SelPrefix -> Symbol -> Symbol -> Constraint
class KnownSymbol name => IndexName prefix tpe name | prefix tpe -> name where

instance (
    name ~ IndexPrefixed prefixSpec tpe,
    KnownSymbol name
  ) => IndexName prefixSpec tpe name where

type family TypePrefixed (spec :: SelPrefix) (name :: Symbol) :: Symbol where
  TypePrefixed 'DefaultPrefix name = AppendSymbol "sqel_type__" name
  TypePrefixed 'NoPrefix name = name
  TypePrefixed ('SelPrefix spec) name = AppendSymbol spec name

type TypeName :: SelPrefix -> Symbol -> Symbol -> Constraint
class (
    KnownSymbol name,
    KnownSymbol tpe
  ) => TypeName prefix tpe name | prefix tpe -> name where

instance (
    name ~ TypePrefixed prefixSpec tpe,
    KnownSymbol name,
    KnownSymbol tpe
  ) => TypeName prefixSpec tpe name where

data Sel =
  SelSymbol Symbol
  |
  SelPath [Symbol]
  |
  SelAuto
  |
  SelUnused
  |
  SelIndex SelPrefix Symbol
  |
  SelType SelPrefix Symbol

type SelW :: Sel -> Type
data SelW sel where
  SelWSymbol :: KnownSymbol name => Proxy name -> SelW ('SelSymbol name)
  SelWPath :: SelW ('SelPath path)
  SelWAuto :: SelW 'SelAuto
  SelWUnused :: SelW 'SelUnused
  SelWIndex :: IndexName prefix tpe name => Proxy name -> SelW ('SelIndex prefix tpe)
  SelWType :: TypeName prefix tpe name => Proxy '(tpe, name) -> SelW ('SelType prefix tpe)

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

instance (
    TypeName prefix tpe name
  ) => MkSel ('SelType prefix tpe) where
  mkSel = SelWType Proxy

-- TODO path: store witness
showSelW :: SelW s -> Text
showSelW = \case
  SelWAuto -> "<auto>"
  SelWPath -> "<path>"
  SelWUnused -> "<unused>"
  SelWSymbol (Proxy :: Proxy sel) -> symbolText @sel
  SelWIndex (Proxy :: Proxy sel) -> [exon|<index for #{symbolText @sel}>|]
  SelWType (Proxy :: Proxy '(tpe, name)) -> [exon|<type name for #{symbolText @name}>|]

type ReifySel :: Sel -> Symbol -> Constraint
class KnownSymbol name => ReifySel sel name | sel -> name where
  reifySel :: SelW sel -> Text

instance KnownSymbol name => ReifySel ('SelSymbol name) name where
  reifySel (SelWSymbol Proxy) = symbolText @name

instance (
    IndexName prefixSpec sel name
  ) => ReifySel ('SelIndex prefixSpec sel) name where
  reifySel (SelWIndex Proxy) = symbolText @name

instance (
    TypeName prefixSpec sel name
  ) => ReifySel ('SelType prefixSpec sel) name where
  reifySel (SelWType Proxy) = symbolText @name
