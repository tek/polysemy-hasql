module Sqel.Type where

import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (ConstructorInfo (Record), DatatypeInfo (ADT), FieldInfo (FieldInfo))
import Prelude hiding (Mod)

import qualified Sqel.Data.Dd as Kind
import Sqel.Data.Dd (DdK (DdK), Struct (Comp))
import Sqel.Data.Mods (Newtype, NoMods)
import Sqel.Data.Sel (Sel (SelAuto, SelSymbol, SelType, SelUnused), SelPrefix (DefaultPrefix))
import Sqel.Data.Select (SelectAtom)
import Sqel.SOP.Constraint (DataNameF)
import Sqel.SOP.Error (QuotedType)

type family Prod (a :: Type) :: DdK where
  Prod a =
    'DdK 'SelAuto NoMods a ('Comp ('SelType 'DefaultPrefix (DataNameF a)) ('Kind.Prod 'Kind.Reg) 'Kind.Nest '[])

type family Merge (dd :: DdK) :: DdK where
  Merge ('DdK sel mods a ('Comp tsel c _ sub)) = 'DdK sel mods a ('Comp tsel c 'Kind.Merge sub)
  Merge s = s

-- TODO this could accept the type on the lhs and call Prod
type (*>) :: DdK -> k -> DdK
type family (*>) base sub

type instance ('DdK sel mods a ('Comp tsel c i '[])) *> (sub :: [DdK]) =
  'DdK sel mods a ('Comp tsel c i sub)

type instance ('DdK sel mods a ('Comp tsel c i '[])) *> (sub :: DdK) =
  'DdK sel mods a ('Comp tsel c i '[sub])

infix 4 *>

type (>) :: DdK -> k -> [DdK]
type family (>) a b
type instance a > (b :: [DdK]) = a : b
type instance a > (b :: DdK) = [a, b]

infixr 5 >

type family PrimSel (sel :: Sel) (a :: Type) :: DdK where
  PrimSel sel a = 'DdK sel NoMods a 'Kind.Prim

type family PrimUnused (a :: Type) :: DdK where
  PrimUnused a = PrimSel 'SelUnused a

type family Prim (name :: Symbol) (a :: Type) :: DdK where
  Prim name a = PrimSel ('SelSymbol name) a

type family NewtypeWrapped' (a :: Type) (ass :: [[Type]]) :: Type where
  NewtypeWrapped' _ '[ '[w]] = w
  NewtypeWrapped' a _ = TypeError (QuotedType a <> " is not a newtype.")

type family NewtypeWrapped (a :: Type) :: Type where
  NewtypeWrapped a = NewtypeWrapped' a (GCode a)

type family PrimNewtype (name :: Symbol) (a :: Type) :: DdK where
  PrimNewtype name a = Mod (Newtype a (NewtypeWrapped a)) (Prim name a)

type family Name (name :: Symbol) (dd :: DdK) :: DdK where
  Name name ('DdK _ mods a s) =
    'DdK ('SelSymbol name) mods a s

-- TODO this will result in a double prefix when used with DdTypeName and manually set prefix
type family TypeName (name :: Symbol) (dd :: DdK) :: DdK where
  TypeName name ('DdK sel mods a ('Comp _ c i sub)) =
    'DdK sel mods a ('Comp ('SelType 'DefaultPrefix name) c i sub)

type family ProdPrimFields (as :: [Type]) (fields :: [FieldInfo]) :: [DdK] where
  ProdPrimFields '[] '[] = '[]
  ProdPrimFields (a : as) ('FieldInfo name : fields) =
    Prim name a : ProdPrimFields as fields

type family ProdPrims' (a :: Type) (code :: [[Type]]) (info :: DatatypeInfo) :: DdK where
  ProdPrims' a '[as] ('ADT _ name '[ 'Record _ fields] _) =
    'DdK 'SelAuto NoMods a ('Comp ('SelType 'DefaultPrefix name) ('Kind.Prod 'Kind.Reg) 'Kind.Nest (ProdPrimFields as fields))

type family ProdPrims (a :: Type) :: DdK where
  ProdPrims a = ProdPrims' a (GCode a) (GDatatypeInfoOf a)

type family ProdPrimNewtypeFields (as :: [Type]) (fields :: [FieldInfo]) :: [DdK] where
  ProdPrimNewtypeFields '[] '[] = '[]
  ProdPrimNewtypeFields (a : as) ('FieldInfo name : fields) =
    PrimNewtype name a : ProdPrimNewtypeFields as fields

type family ProdPrimsNewtype' (a :: Type) (code :: [[Type]]) (info :: DatatypeInfo) :: DdK where
  ProdPrimsNewtype' a '[as] ('ADT _ name '[ 'Record _ fields] _) =
    'DdK 'SelAuto NoMods a ('Comp ('SelType 'DefaultPrefix name) ('Kind.Prod 'Kind.Reg) 'Kind.Nest (ProdPrimNewtypeFields as fields))

type family ProdPrimsNewtype (a :: Type) :: DdK where
  ProdPrimsNewtype a = ProdPrimsNewtype' a (GCode a) (GDatatypeInfoOf a)

type family Mods (mods :: [Type]) (dd :: DdK) :: DdK where
  Mods new ('DdK sel old a s) = 'DdK sel (new ++ old) a s

type family ModsR (mods :: [Type]) (dd :: DdK) :: DdK where
  ModsR new ('DdK sel old a s) = 'DdK sel (old ++ new) a s

type family Mod (mod :: Type) (dd :: DdK) :: DdK where
  Mod mod dd = Mods '[mod] dd

type family MSelect (dd :: DdK) :: DdK where
  MSelect dd = Mod SelectAtom dd
