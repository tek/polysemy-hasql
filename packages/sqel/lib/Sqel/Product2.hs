module Sqel.Product2 where

import Fcf (Length, type (@@))
import Generics.SOP (NP (Nil, (:*)))
import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (
  ConstructorInfo (Constructor, Infix, Record),
  DatatypeInfo (ADT),
  FieldInfo (FieldInfo),
  )
import Prelude hiding (sum, type (@@))
import Type.Errors (DelayError, ErrorMessage (Text))

import Sqel.Comp (CompName (compName))
import Sqel.Data.Dd (
  Comp (Prod, Sum),
  CompInc (Merge, Nest),
  ConCol,
  Dd (Dd),
  DdInc (DdMerge, DdNest),
  DdK (DdK),
  DdStruct (DdComp),
  DdType,
  DdVar (DdCon, DdProd, DdSum),
  ProdType (Con, Reg),
  ProductField (ProductField),
  Struct (Comp, Prim),
  type (:>) ((:>)),
  )
import Sqel.Data.Mods (pattern NoMods, NoMods)
import Sqel.Data.Sel (MkSel (mkSel), Sel (SelAuto, SelSymbol, SelUnused), SelW (SelWAuto))
import Sqel.Merge (merge)
import Sqel.Names.Data (NatSymbol)
import Sqel.Names.Error (CountMismatch)
import Sqel.Names.Rename (Rename (rename))
import Sqel.Names.Set (SetName)
import Sqel.Prim (IndexColumn, Prims (Prims), primIndex)
import qualified Sqel.Type as T

type family RecordFields (fields :: [FieldInfo]) (ass :: [Type]) :: [ProductField] where
  RecordFields '[] '[] = '[]
  RecordFields ('FieldInfo name : fields) (a : as) = 'ProductField name a : RecordFields fields as

type family ConstructorFields (name :: Symbol) (index :: Nat) (ass :: [Type]) :: [ProductField] where
  ConstructorFields _ _ '[] = '[]
  ConstructorFields name n (a : as) = 'ProductField (AppendSymbol name (NatSymbol n)) a : ConstructorFields name (n + 1) as

type family ProductFields (info :: DatatypeInfo) (ass :: [[Type]]) :: [ProductField] where
  ProductFields ('ADT _ _ '[ 'Record _ fields] _) '[ass] = RecordFields fields ass

-- TODO check if the sel cases can be refactored into another class that does the rename/id distinction
-- First add an error test case for the higher-order constraint of ProductIten
class ProductItem field arg s | field arg -> s where
  productItem :: arg -> Dd s

instance (
    a ~ b,
    KnownSymbol name
  ) => ProductItem ('ProductField name a) (Dd ('DdK 'SelAuto mods b 'Prim)) ('DdK ('SelSymbol name) mods a 'Prim) where
  productItem = rename

instance (
    a ~ b
  ) => ProductItem ('ProductField fname a) (Dd ('DdK ('SelSymbol name) mods b 'Prim)) ('DdK ('SelSymbol name) mods a 'Prim) where
  productItem = id

instance (
    a ~ b
  ) => ProductItem ('ProductField name a) (Dd ('DdK 'SelUnused mods b 'Prim)) ('DdK 'SelUnused mods a 'Prim) where
  productItem = id

instance (
    a ~ b,
    KnownSymbol name
  ) => ProductItem ('ProductField name a) (Dd ('DdK 'SelAuto mods b ('Comp tsel c 'Nest s))) ('DdK ('SelSymbol name) mods a ('Comp tsel c 'Nest s)) where
  productItem = rename

instance (
    a ~ b
  ) => ProductItem ('ProductField fname a) (Dd ('DdK ('SelSymbol name) mods b ('Comp tsel c 'Nest s))) ('DdK ('SelSymbol name) mods a ('Comp tsel c 'Nest s)) where
  productItem = id

instance (
    a ~ b
  ) => ProductItem ('ProductField name a)(Dd ('DdK 'SelAuto mods b ('Comp tsel c 'Merge s))) ('DdK 'SelAuto mods a ('Comp tsel c 'Merge s)) where
  productItem = id

data CompMeta =
  CompMeta {
    desc :: Symbol,
    name :: ErrorMessage,
    combinator :: Symbol,
    index :: Nat
  }
  deriving stock (Generic)

type family MetaNext (meta :: CompMeta) :: CompMeta where
  MetaNext ('CompMeta desc name combinator index) = 'CompMeta desc name combinator (index + 1)

type family MetaFor (desc :: Symbol) (name :: ErrorMessage) (combinator :: Symbol) :: CompMeta where
  MetaFor desc name combinator = 'CompMeta desc name combinator 1

data SpecType = SpecNP | SpecDsl | SpecPrims

type family CheckFields (meta :: CompMeta) (len :: Nat) (fieldLen :: Nat) (t :: SpecType) :: Either Void SpecType where
  CheckFields _ n n t = 'Right t
  CheckFields ('CompMeta desc name _ _) arg f _ = 'Left (DelayError (CountMismatch desc name arg f))

type family DslSize (arg :: Type) :: Nat where
  DslSize (_ :> as) = 1 + DslSize as
  DslSize _ = 1

type family TriageComp (meta :: CompMeta) (arg :: Type) (fields :: [ProductField]) :: Either Void SpecType where
  TriageComp _ (Prims _ _) _ = 'Right 'SpecPrims
  TriageComp meta (NP _ s) fs = CheckFields meta (Length @@ s) (Length @@ fs) 'SpecNP
  TriageComp meta args fs = CheckFields meta (DslSize args) (Length @@ fs) 'SpecDsl

type CompColumn' :: CompMeta -> Either Void SpecType -> [ProductField] -> Type -> Type -> [DdK] -> Constraint
class CompColumn' meta spec fields a arg s | fields arg -> s where
  compColumn' :: arg -> NP Dd s

instance CompColumn' meta ('Right 'SpecNP) '[] a (NP f '[]) '[] where
  compColumn' Nil = Nil

instance (
    ProductItem field (f arg0) s0,
    CompColumn' (MetaNext meta) ('Right 'SpecNP) fields a (NP f args) s1
  ) => CompColumn' meta ('Right 'SpecNP) (field : fields) a (NP f (arg0 : args)) (s0 : s1) where
    compColumn' (arg0 :* args) =
      productItem @field arg0 :* compColumn' @(MetaNext meta) @('Right 'SpecNP) @fields @a args

instance (
    ProductItem field arg0 s0,
    CompColumn' (MetaNext meta) ('Right 'SpecDsl) fields a args s1
  ) => CompColumn' meta ('Right 'SpecDsl) (field : fields) a (arg0 :> args) (s0 : s1) where
  compColumn' (arg0 :> args) =
    productItem @field arg0 :* compColumn' @(MetaNext meta) @('Right 'SpecDsl) @fields @a args

instance (
    ProductItem field arg s
  ) => CompColumn' meta ('Right 'SpecDsl) '[field] a arg '[s] where
    compColumn' arg = productItem @field arg :* Nil

instance (
    a ~ b,
    CompColumn' meta ('Right 'SpecNP) fields a (NP Dd s0) s1
  ) => CompColumn' meta ('Right 'SpecPrims) fields a (Prims b s0) s1 where
  compColumn' (Prims np) = compColumn' @meta @('Right 'SpecNP) @fields @a np

type CompColumn :: CompMeta -> [ProductField] -> Type -> Type -> [DdK] -> Constraint
class CompColumn meta fields a arg s | fields arg -> s where
  compColumn :: arg -> NP Dd s

instance (
    spec ~ TriageComp meta arg fields,
    CompColumn' meta spec fields a arg s
  ) => CompColumn meta fields a arg s where
    compColumn = compColumn' @meta @spec @fields @a

class DdType s ~ a => Product2 a arg s | a arg -> s where
  prod :: arg -> Dd s

instance (
    CompName a sel,
    fields ~ ProductFields (GDatatypeInfoOf a) (GCode a),
    meta ~ MetaFor "product type" ('ShowType a) "prod",
    CompColumn meta fields a arg s
  ) => Product2 a arg ('DdK 'SelAuto NoMods a ('Comp sel ('Prod 'Reg) 'Nest s)) where
    prod arg =
      Dd SelWAuto NoMods (DdComp (compName @a) DdProd DdNest (compColumn @meta @fields @a arg))

prodAs ::
  ∀ (name :: Symbol) (a :: Type) (s :: DdK) (arg :: Type) .
  Product2 a arg s =>
  Rename s (SetName s name) =>
  arg ->
  Dd (SetName s name)
prodAs =
  rename . prod @_ @_ @s

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

class DdType s ~ a => Sum2 a arg s | a arg -> s where
  sum :: arg -> Dd s

sumAs ::
  ∀ (name :: Symbol) (a :: Type) (s :: DdK) (arg :: Type) .
  Sum2 a arg s =>
  Rename s (SetName s name) =>
  arg ->
  Dd (SetName s name)
sumAs =
  rename . sum @a @_ @s

mergeSum ::
  ∀ (a :: Type) (s :: DdK) (arg :: Type) .
  Sum2 a arg s =>
  arg ->
  Dd (T.Merge s)
mergeSum =
  merge . sum @a @_ @s

-- TODO b ~ a is not needed here, apparently, but it is for ConColumn. investigate and remove here
instance (
    b ~ a,
    CompName a ('SelSymbol name),
    KnownSymbol name,
    fields ~ SumFields (GDatatypeInfoOf a) (GCode a),
    meta ~ MetaFor "sum type" ('ShowType a) "sum",
    CompColumn meta fields a arg s
  ) => Sum2 b arg ('DdK 'SelAuto NoMods a ('Comp ('SelSymbol name) 'Sum 'Nest (IndexColumn name : s))) where
  sum arg =
    Dd SelWAuto NoMods (DdComp (compName @a) DdSum DdNest (primIndex :* compColumn @meta @fields @a arg))

class DdType s ~ a => ConColumn a arg s | a arg -> s where
  con :: arg -> Dd s

instance (
    a ~ ConCol name record fields as,
    KnownSymbol name,
    meta ~ MetaFor "constructor" ('Text name) "con",
    CompColumn meta fields a arg s
  ) => ConColumn a arg ('DdK 'SelAuto NoMods (ConCol name record fields as) ('Comp ('SelSymbol name) ('Prod ('Con as)) 'Nest s)) where
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
    KnownSymbol name,
    meta ~ MetaFor "constructor" ('Text name) "con1",
    CompColumn meta (Con1Fields a) a arg s
  ) => Con1Column a arg ('DdK 'SelAuto NoMods (ConCol name record fields as) ('Comp ('SelSymbol name) ('Prod ('Con as)) 'Merge s)) where
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
    KnownSymbol name,
    fields ~ Con1Fields (RenameCon1 name a),
    meta ~ MetaFor "constructor" ('Text name) "con1As",
    CompColumn meta fields a arg s
  ) => Con1AsColumn name a arg ('DdK 'SelAuto NoMods a ('Comp ('SelSymbol name) ('Prod ('Con as)) 'Merge s)) where
  con1As arg =
    Dd SelWAuto NoMods (DdComp mkSel DdCon DdMerge (compColumn @meta @fields @a arg))
