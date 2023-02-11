module Sqel.Data.Dd where

import Generics.SOP (I, NP (Nil, (:*)))
import Prettyprinter (Doc, Pretty (pretty), brackets, nest, parens, vsep, (<+>))

import Sqel.Data.Mods (Mods)
import Sqel.Data.Sel (Sel (SelSymbol), SelW, TSel, TSelW, showSelW, showTSelW)

data ProductField =
  ProductField {
    name :: Symbol,
    tpe :: Type
  }

newtype ConCol (name :: Symbol) (record :: Bool) (fields :: [ProductField]) as =
  ConCol { unConCol :: NP I as }

data ProdType = Reg | Con [Type]

data Comp = Prod ProdType | Sum

data CompInc = Merge | Nest

data Struct =
  Prim
  |
  Comp {
    typeName :: TSel,
    compKind :: Comp,
    compInc :: CompInc,
    sub :: [DdK]
  }

data DdK =
  DdK {
    columnName :: Sel,
    mods :: [Type],
    hsType :: Type,
    struct :: Struct
  }

type DdSort :: Comp -> Type
data DdSort c where
  DdProd :: DdSort ('Prod 'Reg)
  DdCon :: DdSort ('Prod ('Con as))
  DdSum :: DdSort 'Sum

type MkDdSort :: Comp -> Constraint
class MkDdSort c where ddSort :: DdSort c

instance MkDdSort ('Prod 'Reg) where ddSort = DdProd
instance MkDdSort ('Prod ('Con as)) where ddSort = DdCon
instance MkDdSort 'Sum where ddSort = DdSum

type DdInc :: CompInc -> Type
data DdInc c where
  DdMerge :: DdInc 'Merge
  DdNest :: DdInc 'Nest

type MkDdInc :: CompInc -> Constraint
class MkDdInc c where ddInc :: DdInc c

instance MkDdInc 'Merge where ddInc = DdMerge
instance MkDdInc 'Nest where ddInc = DdNest

type DdStruct :: Struct -> Type
data DdStruct s where
  DdPrim :: DdStruct 'Prim
  DdComp :: TSelW sel -> DdSort c -> DdInc i -> NP Dd sub -> DdStruct ('Comp sel c i sub)

-- TODO maybe this could be a data family so that after using the dsl, the index is changed so that all Sels are present
-- also to stuff different metadata in there, like DdlColumn?
type Dd :: DdK -> Type
data Dd s where
  Dd :: SelW sel -> Mods mods -> DdStruct s -> Dd ('DdK sel mods a s)

data QOp =
  QAnd
  |
  QOr
  deriving stock (Eq, Show, Generic)

type DdType :: DdK -> Type
type family DdType s where
  DdType ('DdK _ _ a _) = a

type DdTypes :: [DdK] -> [Type]
type family DdTypes s where
  DdTypes '[] = '[]
  DdTypes (s : ss) = DdType s : DdTypes ss

type DdSel :: DdK -> Sel
type family DdSel s where
  DdSel ('DdK sel _ _ _) = sel

type family DdName (s :: DdK) :: Symbol where
  DdName ('DdK ('SelSymbol name) _ _ _) = name
  DdName ('DdK _ _ a _) = TypeError ("This Dd for type " <> a <> " has no name")

type DdTypeSel :: DdK -> TSel
type family DdTypeSel s where
  DdTypeSel ('DdK _ _ _ ('Comp sel _ _ _)) = sel

sel :: Dd s -> SelW (DdSel s)
sel (Dd s _ _) = s

typeSel :: Dd ('DdK sel p a ('Comp tsel c i sub)) -> TSelW tsel
typeSel (Dd _ _ (DdComp s _ _ _)) = s

showSel :: Dd s -> Text
showSel =
  showSelW . sel

showTypeSel :: Dd ('DdK sel p a ('Comp tsel c i sub)) -> Text
showTypeSel =
  showTSelW . typeSel

data a :> b = a :> b
infixr 3 :>

class PrettyNP s where
  prettyNP :: NP Dd s -> [Doc ann]

instance PrettyNP '[] where
  prettyNP Nil = mempty

instance (
    Pretty (Dd s),
    PrettyNP ss
  ) => PrettyNP (s : ss) where
  prettyNP (dd :* dds) =
    pretty dd : prettyNP dds

instance Pretty (DdStruct 'Prim) where
  pretty DdPrim = "prim"

instance (
    PrettyNP sub
  ) => Pretty (Dd ('DdK sel p a ('Comp tsel c i sub))) where
  pretty (Dd s _ (DdComp ts c i sub)) =
    nest 2 (vsep ((var <> brackets (pretty (showTSelW ts)) <+> pretty (showSelW s) <+> parens inc) : prettyNP sub))
    where
      var = case c of
        DdProd -> "prod"
        DdSum -> "sum"
        DdCon -> "con"
      inc = case i of
        DdNest -> "nest"
        DdMerge -> "merge"

instance (
    Pretty (Mods p)
  ) => Pretty (Dd ('DdK sel p a 'Prim)) where
  pretty (Dd s p DdPrim) =
    "prim" <+> pretty (showSelW s) <+> pretty p

type Sqel' :: Sel -> [Type] -> Type -> Struct -> Type
type family Sqel' sel mods a s = r | r -> sel mods a s where
  Sqel' sel mods a s = Dd ('DdK sel mods a s)

type Sqel :: Type -> (Sel, [Type], Struct) -> Type
type family Sqel a p = r | r -> p a where
  Sqel a '(sel, mods, s) = Dd ('DdK sel mods a s)
