module Sqel.Product where

import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Prelude hiding (sum, type (@@))

import Sqel.Comp (CompColumn (compColumn), CompName, MetaFor, ProductFields)
import Sqel.Data.Dd (
  Comp (Prod),
  CompInc (Nest),
  Dd (Dd),
  DdInc (DdNest),
  DdK (DdK),
  DdSort (DdProd),
  DdStruct (DdComp),
  DdType,
  ProdType (Reg),
  Struct (Comp),
  )
import Sqel.Data.Mods (pattern NoMods, NoMods)
import Sqel.Data.Sel (MkTSel (mkTSel), Sel (SelAuto), SelW (SelWAuto))
import Sqel.Names.Rename (Rename (rename))
import Sqel.Names.Set (SetName)

class DdType s ~ a => ProductSel sel a arg s | sel a arg -> s where
  prodSel :: arg -> Dd s

instance (
    MkTSel sel,
    fields ~ ProductFields (GDatatypeInfoOf a) (GCode a),
    meta ~ MetaFor "product type" ('ShowType a) "prod",
    CompColumn meta fields a arg s
  ) => ProductSel sel a arg ('DdK 'SelAuto NoMods a ('Comp sel ('Prod 'Reg) 'Nest s)) where
    prodSel arg =
      Dd SelWAuto NoMods (DdComp mkTSel DdProd DdNest (compColumn @meta @fields @a arg))

class DdType s ~ a => Product a arg s | a arg -> s where
  prod :: arg -> Dd s

instance (
    CompName a sel,
    ProductSel sel a arg s
  ) => Product a arg s where
    prod =
      prodSel @sel

prodAs ::
  ∀ (name :: Symbol) (a :: Type) (s :: DdK) (arg :: Type) .
  Product a arg s =>
  Rename s (SetName s name) =>
  arg ->
  Dd (SetName s name)
prodAs =
  rename . prod @_ @_ @s
