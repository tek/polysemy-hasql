module Polysemy.Db.Tree where

import Generics.SOP (NP(Nil, (:*)))

import Polysemy.Db.Data.Column (Auto, Con, Product, Rep)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (DataName, symbolText)
import Polysemy.Db.Text.DbIdentifier (quotedDbId)
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Effect (ResolveTreeEffects)
import Polysemy.Db.Tree.Meta (ADTMetadata (ADTProd, ADTSum), ColumnMeta(ColumnMeta), ColumnMetaType, ConMeta(ConMeta))
import qualified Polysemy.Db.Type.Data.Tree as Type

class TreeImpl t n e where
  type ConParam t n e :: e
  type ColParam t n e (meta :: ColumnMeta) :: e

class TreePrim t n e a (meta :: ColumnMeta) where
  treePrim :: a -> n (ColumnMetaType meta)

class TreePayload t n e a (meta :: ColumnMeta) where
  treePayload :: a -> t

class TreeProduct t n e d prod | t n e d -> prod where
  treeProduct :: d -> prod

class TreeProductElem t n e p pt (meta :: ColumnMeta) a | t n e p meta -> pt a where
  treeProductElem :: p -> (pt, a)

class TreeCons t n e d cons | t n e d -> cons where
  treeCons :: d -> cons

class TreeConElem t n e cons consTail con | t n e cons -> consTail con where
  treeConElem :: cons -> (consTail, con)

class ProdColumns (t :: Type) (n :: Type -> Type) (e :: Type) (p :: Type) (metas :: [ColumnMeta]) (cs :: [Kind.Tree e]) | metas t n e -> cs where
  prodColumns :: p -> NP (Type.Tree t n) cs

instance ProdColumns t n e p '[] '[] where
  prodColumns _ =
    Nil

-- TODO replace with hcmap or so
instance (
    meta ~ 'ColumnMeta name rep a,
    TreeProductElem t n e p pt meta d,
    Tree t n e d meta c,
    ProdColumns t n e pt metas cs
  ) => ProdColumns t n e p (meta : metas) (c : cs) where
    prodColumns p =
      tree @t @n @e @d @meta a :* prodColumns @t @n @e @pt @metas pt
      where
        (pt, a) =
          treeProductElem @t @n @e @p @pt @meta p

class ConColumn (t :: Type) (n :: Type -> Type) (e :: Type) (con :: Type) (meta :: ConMeta) (c :: Kind.Tree e) | meta t n e -> c where
  conColumn :: con -> Type.Tree t n c

instance {-# overlappable #-} (
    TreePayload t n e a meta,
    ProdColumns t n e con cols cs,
    c ~ 'Kind.Tree name (ConParam t n e) ('Kind.Prod (Con name) cs)
  ) => ConColumn t n e con ('ConMeta name cols) c where
    conColumn con =
      Type.Tree (treePayload @t @n @e @a @meta undefined) (Type.Prod (prodColumns @t @n @e @con @cols con))

instance (
    meta ~ 'ColumnMeta cname rep d,
    Tree t n e con meta c
  ) => ConColumn t n e con ('ConMeta cname '[ 'ColumnMeta name rep d]) c where
    conColumn =
      tree @t @n @e @con @meta

class SumColumns (t :: Type) (n :: Type -> Type) (e :: Type) (cons' :: Type) (cons :: [ConMeta]) (cs :: [Kind.Tree e]) | cons t n e -> cs where
  sumColumns :: cons' -> NP (Type.Tree t n) cs

instance SumColumns t n e cons' '[] '[] where
  sumColumns _ =
    Nil

instance (
    TreeConElem t n e cons' consTail con',
    ConColumn t n e con' con c,
    SumColumns t n e consTail cons cs
  ) => SumColumns t n e cons' (con : cons) (c : cs) where
    sumColumns cons =
      conColumn @t @n @e @con' @con con :* sumColumns @t @n @e @consTail @cons conTail
      where
        (conTail, con) =
          treeConElem @t @n @e cons

class ADTColumn (t :: Type) (n :: Type -> Type) (e :: Type) (d :: Type) (meta :: ADTMetadata) (eff :: [*]) (ct :: Type) (sub :: Kind.Node e) | d meta eff ct t n e -> sub where
  adtColumn :: d -> Type.Node t n sub

instance (
    TreeProduct t n e d prod,
    ProdColumns t n e prod cols cs
  ) => ADTColumn t n e d ('ADTProd cols) '[] ct ('Kind.Prod d cs) where
  adtColumn d =
    Type.Prod (prodColumns @t @n @e @prod @cols (treeProduct @t @n @e d))

type SumIndexColumn e =
  'Kind.Tree ('NamedField "sum_index") e ('Kind.Prim Int)

instance (
    -- TreeImpl t n e,
    -- TreePrim f t n e indexMeta,
    TreeCons t n e d cons,
    SumColumns t n e cons cols cs,
    -- indexMeta ~ 'ColumnMeta ('NamedField "sum_index") (Rep '[Prim]) Int,
    node ~ 'Kind.Sum d cs
  ) => ADTColumn t n e d ('ADTSum cols) '[] ct node where
  adtColumn d =
    Type.Sum (sumColumns @t @n @e @cons @cols (treeCons @t @n @e d))

class ColumnForKind (meta :: ColumnMeta) (t :: Type) (n :: Type -> Type) (e :: Type) (a :: *) (eff :: [*]) (d :: Type) (ct :: Type) (sub :: Kind.Node e) | eff d ct t n e -> sub where
  columnForKind :: a -> Type.Node t n sub

instance (
    ADTColumn t n e d meta effs ct sub
  ) => ColumnForKind _meta t n e d (ADT meta rep : effs) d ct sub where
    columnForKind =
      adtColumn @t @n @e @d @meta @effs @ct

instance (
    TreePrim t n e a meta,
    meta ~ 'ColumnMeta name rep d
  ) => ColumnForKind meta t n e a '[] d ct ('Kind.Prim d) where
  columnForKind fa =
    Type.Prim (treePrim @t @n @e @a @meta fa)

instance {-# overlappable #-} (
    ColumnForKind meta t n e a effs d ct sub
  ) => ColumnForKind meta t n e a (eff : effs) d ct sub where
    columnForKind =
      columnForKind @meta @t @n @e @a @effs @d @ct

class Tree (t :: Type) (n :: Type -> Type) (e :: Type) (a :: Type) (meta :: ColumnMeta) (tree :: Kind.Tree e) | meta t n e -> tree where
  tree :: a -> Type.Tree t n tree

instance (
    TreePayload t n e a meta,
    ResolveTreeEffects rep d effs ct,
    meta ~ 'ColumnMeta name rep d,
    ColumnForKind meta t n e a effs d ct sub,
    tree ~ 'Kind.Tree name (ColParam t n e meta) sub
  ) => Tree t n e a meta tree where
    tree a =
      Type.Tree (treePayload @t @n @e @a @meta a) (columnForKind @meta @t @n @e @a @effs @d @ct a)

class KnownSymbol name => TableName (d :: Type) (name :: Symbol) | d -> name where
  tableName :: Text
  tableName =
    quotedDbId (symbolText @name)

instance {-# overlappable #-} (
    KnownSymbol name,
    DataName d name
  ) => TableName d name

instance TableName d name => TableName (Uid i d) name where

class TableMeta (d :: Type) (meta :: ColumnMeta) | d -> meta

instance {-# overlappable #-} (
    TableName d name,
    meta ~ 'ColumnMeta ('NamedField name) (Rep '[Product Auto]) d
  ) => TableMeta d meta

class TableTree (t :: Type) (n :: Type -> Type) (e :: Type) (d :: Type) (c :: Kind.Tree e) | d t n e -> c where
  tableTree :: Type.Tree t n c

instance (
    TableMeta d meta,
    Tree t n e d meta c
  ) => TableTree t n e d c where
  tableTree =
    tree @t @n @e @d @meta undefined
