module Sqel.Sum where

import Generics.SOP (NP ((:*)))
import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (ConstructorInfo (Constructor, Infix, Record), DatatypeInfo (ADT))
import Prelude hiding (sum, type (@@))
import Type.Errors (ErrorMessage (Text))

import Sqel.Comp (CompColumn (compColumn), CompName (compName), ConstructorFields, MetaFor, RecordFields)
import Sqel.Data.Dd (
  Comp (Prod, Sum),
  CompInc (Merge, Nest),
  ConCol,
  Dd (Dd),
  DdInc (DdMerge, DdNest),
  DdK (DdK),
  DdStruct (DdComp, DdPrim),
  DdType,
  DdVar (DdCon, DdSum),
  ProdType (Con),
  ProductField (ProductField),
  Struct (Comp, Prim),
  )
import Sqel.Data.Mods (pattern NoMods, NoMods)
import Sqel.Data.Sel (
  IndexName,
  MkSel (mkSel),
  Sel (SelAuto, SelIndex, SelType),
  SelPrefix (DefaultPrefix, SelPrefix),
  SelW (SelWAuto, SelWIndex),
  TypeName,
  )
import Sqel.Merge (merge)
import Sqel.Names.Rename (Rename (rename))
import Sqel.Names.Set (SetName)
import Sqel.Prim (IndexColumn, IndexColumnWith, primIndex)
import qualified Sqel.Type as T

type family SumFields' (fields :: [ConstructorInfo]) (ass :: [[Type]]) :: [ProductField] where
  SumFields' '[] '[] = '[]
  SumFields' ('Record name fields : cons) (as : ass) = 'ProductField name (ConCol name 'True (RecordFields fields as) as) : SumFields' cons ass
  SumFields' ('Constructor name : cons) (as : ass) = 'ProductField name (ConCol name 'False (ConstructorFields name 0 as) as) : SumFields' cons ass
  SumFields' ('Infix conName _ _ : _) _ =
    TypeError ("Infix constructor not supported: " <> conName)

type family SumFields (info :: DatatypeInfo) (ass :: [[Type]]) :: [ProductField] where
  SumFields ('ADT _ _ cons _) ass = SumFields' cons ass
  SumFields info _ =
    TypeError ("SumFields:" % info)

class DdType s ~ a => SumWith a isel imods arg s | a isel imods arg -> s where
  sumWith :: Dd ('DdK isel imods Int64 'Prim) -> arg -> Dd s

-- TODO b ~ a is not needed here, apparently, but it is for ConColumn. investigate and remove here
instance (
    b ~ a,
    CompName a ('SelType prefix name),
    fields ~ SumFields (GDatatypeInfoOf a) (GCode a),
    meta ~ MetaFor "sum type" ('ShowType a) "sum",
    CompColumn meta fields a arg s
  ) => SumWith b isel imods arg ('DdK 'SelAuto NoMods a ('Comp ('SelType prefix name) 'Sum 'Nest ('DdK isel imods Int64 'Prim : s))) where
  sumWith index arg =
    Dd SelWAuto NoMods (DdComp (compName @a) DdSum DdNest (index :* compColumn @meta @fields @a arg))

class DdType s ~ a => Sum a arg s | a arg -> s where
  sum :: arg -> Dd s

-- TODO b ~ a is not needed here, apparently, but it is for ConColumn. investigate and remove here
instance (
    b ~ a,
    CompName a ('SelType prefix name),
    IndexName 'DefaultPrefix name iname,
    fields ~ SumFields (GDatatypeInfoOf a) (GCode a),
    meta ~ MetaFor "sum type" ('ShowType a) "sum",
    CompColumn meta fields a arg s
  ) => Sum b arg ('DdK 'SelAuto NoMods a ('Comp ('SelType prefix name) 'Sum 'Nest (IndexColumn name : s))) where
  sum =
    sumWith primIndex

sumAs ::
  ∀ (name :: Symbol) (a :: Type) (s :: DdK) (arg :: Type) .
  Sum a arg s =>
  Rename s (SetName s name) =>
  arg ->
  Dd (SetName s name)
sumAs =
  rename . sum @a @_ @s

mergeSum ::
  ∀ (a :: Type) (s :: DdK) (arg :: Type) .
  Sum a arg s =>
  arg ->
  Dd (T.Merge s)
mergeSum =
  merge . sum @a @_ @s

class DdType s ~ a => ConColumn a arg s | a arg -> s where
  con :: arg -> Dd s

instance (
    a ~ ConCol name record fields as,
    MkSel ('SelType 'DefaultPrefix name),
    meta ~ MetaFor "constructor" ('Text name) "con",
    CompColumn meta fields a arg s
  ) => ConColumn a arg ('DdK 'SelAuto NoMods (ConCol name record fields as) ('Comp ('SelType 'DefaultPrefix name) ('Prod ('Con as)) 'Nest s)) where
  con arg =
    Dd SelWAuto NoMods (DdComp mkSel DdCon DdNest (compColumn @meta @fields @(ConCol name record fields as) arg))

conAs ::
  ∀ (name :: Symbol) (a :: Type) (s :: DdK) (arg :: Type) .
  ConColumn a arg s =>
  Rename s (SetName s name) =>
  arg ->
  Dd (SetName s name)
conAs =
  rename . con @a @_ @s

type family Con1Fields (con :: Type) :: [ProductField] where
  Con1Fields (ConCol _ 'True '[f] _) = '[f]
  Con1Fields (ConCol name 'False '[ 'ProductField _ a] _) = '[ 'ProductField name a]

class DdType s ~ a => Con1Column a arg s | a arg -> s where
  con1 :: arg -> Dd s

instance (
    a ~ ConCol name record fields as,
    TypeName 'DefaultPrefix name tname,
    meta ~ MetaFor "constructor" ('Text name) "con1",
    CompColumn meta (Con1Fields a) a arg s
  ) => Con1Column a arg ('DdK 'SelAuto NoMods (ConCol name record fields as) ('Comp ('SelType 'DefaultPrefix name) ('Prod ('Con as)) 'Merge s)) where
  con1 arg =
    Dd SelWAuto NoMods (DdComp mkSel DdCon DdMerge (compColumn @meta @(Con1Fields a) @(ConCol name record fields as) arg))

type family RenameCon1 (name :: Symbol) (a :: Type) :: Type where
  RenameCon1 name (ConCol _ record '[ 'ProductField _ a] as) =
    ConCol name record '[ 'ProductField name a] as
  RenameCon1 _ a =
    TypeError ("RenameCon1:" % a)

class DdType s ~ a => Con1AsColumn name a arg s | name a arg -> s where
  con1As :: arg -> Dd s

instance (
    a ~ ConCol _name record _fields as,
    TypeName 'DefaultPrefix name tname,
    fields ~ Con1Fields (RenameCon1 name a),
    meta ~ MetaFor "constructor" ('Text name) "con1As",
    CompColumn meta fields a arg s
  ) => Con1AsColumn name a arg ('DdK 'SelAuto NoMods a ('Comp ('SelType 'DefaultPrefix name) ('Prod ('Con as)) 'Merge s)) where
  con1As arg =
    Dd SelWAuto NoMods (DdComp mkSel DdCon DdMerge (compColumn @meta @fields @a arg))

type SetIndexPrefix :: Symbol -> DdK -> DdK -> Constraint
class SetIndexPrefix prefix s0 s1 | prefix s0 -> s1 where
  setIndexPrefix :: Dd s0 -> Dd s1

instance (
    IndexName ('SelPrefix prefix) tpe iname
  ) => SetIndexPrefix prefix ('DdK sel mods a ('Comp tsel 'Sum i ('DdK ('SelIndex oldPrefix tpe) NoMods Int64 'Prim : cons))) ('DdK sel mods a ('Comp tsel 'Sum i (IndexColumnWith ('SelPrefix prefix) tpe : cons))) where
    setIndexPrefix (Dd sel mods (DdComp tsel DdSum i (Dd (SelWIndex Proxy) NoMods DdPrim :* cons))) =
      Dd sel mods (DdComp tsel DdSum i (Dd (SelWIndex Proxy) NoMods DdPrim :* cons))
    setIndexPrefix _ =
      error "ghc bug?"

indexPrefix ::
  ∀ prefix s0 s1 .
  SetIndexPrefix prefix s0 s1 =>
  Dd s0 ->
  Dd s1
indexPrefix =
  setIndexPrefix @prefix
