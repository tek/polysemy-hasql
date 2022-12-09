module Sqel.Data.Dd where

import Generics.SOP (I, NP (Nil, (:*)))
import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Prettyprinter (Doc, Pretty (pretty), brackets, nest, parens, vsep, (<+>))

import Sqel.Data.Mods (Mods)
import Sqel.Data.Sel (MkSel (mkSel), Sel (SelSymbol), SelW (SelWAuto, SelWPath, SelWSymbol, SelWUnused), showSelW)
import Sqel.Data.Uid (Uid)
import Sqel.SOP.Constraint (IsDataT, symbolText)

newtype ConCol as =
  ConCol { unConCol :: NP I as }

type family ConCoded' (ass :: [[Type]]) :: [Type] where
  ConCoded' '[] = '[]
  ConCoded' (as : ass) = ConCol as : ConCoded' ass

type family ConCoded (a :: Type) :: [Type] where
  ConCoded a = ConCoded' (GCode a)

data ProdType = Reg | Con [Type]

data Comp = Prod ProdType | Sum

data CompInc = Merge | Nest

data Struct =
  Prim
  |
  Comp {
    typeName :: Sel,
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

type DdVar :: Comp -> Type
data DdVar c where
  DdProd :: DdVar ('Prod 'Reg)
  DdCon :: DdVar ('Prod ('Con as))
  DdSum :: DdVar 'Sum

type MkDdVar :: Comp -> Constraint
class MkDdVar c where ddVar :: DdVar c

instance MkDdVar ('Prod 'Reg) where ddVar = DdProd
instance MkDdVar ('Prod ('Con as)) where ddVar = DdCon
instance MkDdVar 'Sum where ddVar = DdSum

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
  DdComp :: SelW sel -> DdVar c -> DdInc i -> NP Dd sub -> DdStruct ('Comp sel c i sub)

-- TODO maybe this could be a data family so that after using the dsl, the index is changed so that all Sels are present
-- also to stuff different metadata in there, like DdlColumn?
type Dd :: DdK -> Type
data Dd s where
  Dd :: SelW sel -> Mods p -> DdStruct s -> Dd ('DdK sel p a s)

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
  DdSel ('DdK s _ _ _) = s

sel :: Dd s -> SelW (DdSel s)
sel (Dd s _ _) =
  s

typeSel :: Dd ('DdK sel p a ('Comp tsel c i sub)) -> SelW tsel
typeSel (Dd _ _ (DdComp s _ _ _)) =
  s

-- TODO path: store witness
showSel :: Dd s -> Text
showSel =
  sel >>> \case
    SelWAuto -> "<auto>"
    SelWPath -> "<path>"
    SelWUnused -> "<unused>"
    SelWSymbol (Proxy :: Proxy sel) -> symbolText @sel

showTypeSel :: Dd ('DdK sel p a ('Comp tsel c i sub)) -> Text
showTypeSel =
  typeSel >>> \case
    SelWAuto -> "<auto>"
    SelWPath -> "<path>"
    SelWUnused -> "<unused>"
    SelWSymbol (Proxy :: Proxy sel) -> symbolText @sel

class MatchDdType s a | s -> a
instance MatchDdType ('DdK sel p a s) a

type DbTypeName :: Type -> Symbol -> Constraint
class DbTypeName a name | a -> name where
  dbTypeName :: SelW ('SelSymbol name)

instance {-# overlappable #-} (
    IsDataT (GDatatypeInfoOf a) name,
    MkSel ('SelSymbol name)
  ) => DbTypeName a name where
    dbTypeName = mkSel

instance DbTypeName a name => DbTypeName (Uid i a) name where
  dbTypeName = dbTypeName @a

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
    nest 2 (vsep ((var <> brackets (pretty (showSelW ts)) <+> pretty (showSelW s) <+> parens inc) : prettyNP sub))
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
