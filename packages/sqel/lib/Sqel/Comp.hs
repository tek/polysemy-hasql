module Sqel.Comp where

import Fcf (Length, type (@@))
import Generics.SOP (NP (Nil, (:*)))
import Generics.SOP.GGP (GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (ConstructorInfo (Record), DatatypeInfo (ADT), FieldInfo (FieldInfo))
import Prelude hiding (sum, type (@@))
import Type.Errors (DelayError, ErrorMessage)

import Sqel.Data.Dd (
  CompInc (Merge, Nest),
  Dd (Dd),
  DdK (DdK),
  DdStruct (DdComp),
  ProductField (ProductField),
  Struct (Comp, Prim),
  type (:>) ((:>)),
  )
import Sqel.Data.Sel (
  MkTSel (mkTSel),
  Sel (SelAuto, SelSymbol, SelUnused),
  SelPrefix (DefaultPrefix, SelPrefix),
  TSel (TSel),
  TSelW (TSelW),
  TypeName,
  )
import Sqel.Data.Uid (Uid)
import Sqel.Names.Data (NatSymbol)
import Sqel.Names.Error (CountMismatch)
import Sqel.Names.Rename (Rename (rename))
import Sqel.Prim (Prims (Prims))
import Sqel.SOP.Error (Quoted, QuotedType)

-- TODO reimplement for new class structure
-- -- TODO this recurses through the entire subtree.
-- -- can we replace this with a check for the index column? i.e. ($1 != 2 or foo = $2)
-- -- TODO this has to be moved to the post builder
-- class GuardSumPrim s where
--   guardSumPrim :: Dd s -> Dd s

-- instance (
--     OverMod SelectAtom ('DdK sel p a 'Prim)
--   ) => GuardSumPrim ('DdK sel p a 'Prim) where
--   guardSumPrim =
--     overMod \case
--       SelectAtom Where code ->
--         SelectAtom Where (\ s i -> [sql|(#{dollar i} is null or #{code s i})|])
--       m -> m

-- instance (
--     All GuardSumPrim sub
--   ) => GuardSumPrim ('DdK sel param a ('Comp tsel ('Prod 'Reg) 'Nest sub)) where
--     guardSumPrim (Dd sel p (DdComp tsel DdProd DdNest sub)) =
--       Dd sel p (DdComp tsel DdProd DdNest (hcmap (Proxy @GuardSumPrim) guardSumPrim sub))

type CompName :: Type -> TSel -> Constraint
class CompName a sel | a -> sel where
  compName :: TSelW sel

type CompNameData :: Type -> DatatypeInfo -> Symbol
type family CompNameData a info where
  CompNameData _ ('ADT _ name _ _) = name
  CompNameData a _ = TypeError ("The type " <> a <> " is not an ADT.")

instance {-# overlappable #-} (
    name ~ CompNameData a (GDatatypeInfoOf a),
    sel ~ 'TSel 'DefaultPrefix name,
    MkTSel sel
  ) => CompName a sel where
    compName = mkTSel

instance CompName a sel => CompName (Uid i a) sel where
  compName = compName @a

type family RecordFields (fields :: [FieldInfo]) (ass :: [Type]) :: [ProductField] where
  RecordFields '[] '[] = '[]
  RecordFields ('FieldInfo name : fields) (a : as) = 'ProductField name a : RecordFields fields as

type family ConstructorFields (name :: Symbol) (index :: Nat) (ass :: [Type]) :: [ProductField] where
  ConstructorFields _ _ '[] = '[]
  ConstructorFields name n (a : as) = 'ProductField (AppendSymbol name (NatSymbol n)) a : ConstructorFields name (n + 1) as

type family ProductFields (info :: DatatypeInfo) (ass :: [[Type]]) :: [ProductField] where
  ProductFields ('ADT _ _ '[ 'Record _ fields] _) '[ass] = RecordFields fields ass

-- TODO check if the sel cases can be refactored into another class that does the rename/id distinction
-- First add an error test case for the higher-order constraint of Column
--
-- Maybe add a class layer before this that separates Dd and others, so that Column is the one in the error when Dd
-- is able to be inferred and the other one has a custom error
type Column :: Type -> Symbol -> DdK -> DdK -> Constraint
class Column a fieldName s0 s | a fieldName s0 -> s where
  compItem :: Dd s0 -> Dd s

instance (
    a ~ b,
    KnownSymbol name
  ) => Column a name ('DdK 'SelAuto mods b 'Prim) ('DdK ('SelSymbol name) mods a 'Prim) where
  compItem = rename

instance (
    a ~ b
  ) => Column a fname ('DdK ('SelSymbol name) mods b 'Prim) ('DdK ('SelSymbol name) mods a 'Prim) where
  compItem = id

instance (
    a ~ b
  ) => Column a name ('DdK 'SelUnused mods b 'Prim) ('DdK 'SelUnused mods a 'Prim) where
  compItem = id

instance (
    a ~ b,
    KnownSymbol name
  ) => Column a name ('DdK 'SelAuto mods b ('Comp tsel c 'Nest s)) ('DdK ('SelSymbol name) mods a ('Comp tsel c 'Nest s)) where
  compItem = rename

instance (
    a ~ b
  ) => Column a fname ('DdK ('SelSymbol name) mods b ('Comp tsel c 'Nest s)) ('DdK ('SelSymbol name) mods a ('Comp tsel c 'Nest s)) where
  compItem = id

instance (
    a ~ b
  ) => Column a name ('DdK 'SelAuto mods b ('Comp tsel c 'Merge s)) ('DdK 'SelAuto mods a ('Comp tsel c 'Merge s)) where
  compItem = id

data CompMeta =
  CompMeta {
    desc :: Symbol,
    name :: ErrorMessage,
    combinator :: Symbol,
    index :: Nat
  }
  deriving stock (Generic)

type InvalidElem :: Symbol -> Nat -> Type -> Void
type InvalidElem name i arg =
  DelayError (
    "Element number " <> i <> " in the call to " <> Quoted name <> " has type " <> QuotedType arg <> "." %
    "Columns should only be constructed with combinators like " <> Quoted "prim" <> ", " <> Quoted "prod" <> "," %
    Quoted "column" <> " that return the proper type, " <> Quoted "Dd" <> "." %
    "Consult the module " <> Quoted "Sqel.Combinators" <> " for the full API."
  )

type CompItemOrError :: Void -> ProductField -> Type -> DdK -> Constraint
class CompItemOrError err field arg s | field arg -> s where
  compItemOrError :: Proxy err -> arg -> Dd s

instance (
    Column a fieldName s0 s1
  ) => CompItemOrError err ('ProductField fieldName a) (Dd s0) s1 where
    compItemOrError Proxy = compItem @a @fieldName

type CheckCompItem :: CompMeta -> ProductField -> Type -> DdK -> Constraint
class CheckCompItem meta field arg s | field arg -> s where
  checkCompItem :: arg -> Dd s

instance (
    meta ~ 'CompMeta desc name combinator index,
    error ~ InvalidElem combinator index arg,
    CompItemOrError error field arg s1
  ) => CheckCompItem meta field arg s1 where
    checkCompItem = compItemOrError @error @field Proxy

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
    CheckCompItem meta field (f arg0) s0,
    CompColumn' (MetaNext meta) ('Right 'SpecNP) fields a (NP f args) s1
  ) => CompColumn' meta ('Right 'SpecNP) (field : fields) a (NP f (arg0 : args)) (s0 : s1) where
    compColumn' (arg0 :* args) =
      checkCompItem @meta @field arg0 :* compColumn' @(MetaNext meta) @('Right 'SpecNP) @fields @a args

instance (
    CheckCompItem meta field arg0 s0,
    CompColumn' (MetaNext meta) ('Right 'SpecDsl) fields a args s1
  ) => CompColumn' meta ('Right 'SpecDsl) (field : fields) a (arg0 :> args) (s0 : s1) where
  compColumn' (arg0 :> args) =
    checkCompItem @meta @field arg0 :* compColumn' @(MetaNext meta) @('Right 'SpecDsl) @fields @a args

instance (
    CheckCompItem meta field arg s
  ) => CompColumn' meta ('Right 'SpecDsl) '[ field] a arg '[s] where
    compColumn' arg = checkCompItem @meta @field arg :* Nil

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

type SetTypePrefix :: Symbol -> DdK -> DdK -> Constraint
class SetTypePrefix prefix s0 s1 | prefix s0 -> s1 where
  setTypePrefix :: Dd s0 -> Dd s1

instance (
    TypeName ('SelPrefix prefix) tpe tname
  ) => SetTypePrefix prefix ('DdK sel mods a ('Comp ('TSel oldPrefix tpe) c i s)) ('DdK sel mods a ('Comp ('TSel ('SelPrefix prefix) tpe) c i s)) where
    setTypePrefix (Dd sel mods (DdComp _ c i s)) =
      Dd sel mods (DdComp (TSelW Proxy) c i s)

typePrefix ::
  ∀ prefix s0 s1 .
  SetTypePrefix prefix s0 s1 =>
  Dd s0 ->
  Dd s1
typePrefix =
  setTypePrefix @prefix
