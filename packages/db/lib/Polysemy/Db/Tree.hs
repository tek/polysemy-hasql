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

class TreePrim t n a (meta :: ColumnMeta) where
  treePrim :: a -> n (ColumnMetaType meta)

class TreePayload t n a (meta :: ColumnMeta) where
  treePayload :: a -> t

class TreeProduct t n d prod | t n d -> prod where
  treeProduct :: d -> prod

class TreeProductElem t n p pt (meta :: ColumnMeta) a | t n p meta -> pt a where
  treeProductElem :: p -> (pt, a)

class TreeCons t n d cons | t n d -> cons where
  treeCons :: d -> cons

class TreeConElem t n cons consTail con | t n cons -> consTail con where
  treeConElem :: cons -> (consTail, con)

class ProdColumns (t :: Type) (n :: Type -> Type) (p :: Type) (metas :: [ColumnMeta]) (cs :: [Kind.Tree]) | metas t n -> cs where
  prodColumns :: p -> NP (Type.Tree t n) cs

instance ProdColumns t n p '[] '[] where
  prodColumns _ =
    Nil

-- TODO replace with hcmap or so
instance (
    meta ~ 'ColumnMeta name rep a,
    TreeProductElem t n p pt meta d,
    Tree t n d meta c,
    ProdColumns t n pt metas cs
  ) => ProdColumns t n p (meta : metas) (c : cs) where
    prodColumns p =
      tree @t @n @d @meta a :* prodColumns @t @n @pt @metas pt
      where
        (pt, a) =
          treeProductElem @t @n @p @pt @meta p

class ConColumn (t :: Type) (n :: Type -> Type) (con :: Type) (meta :: ConMeta) (c :: Kind.Tree) | meta t n -> c where
  conColumn :: con -> Type.Tree t n c

instance {-# overlappable #-} (
    TreePayload t n a meta,
    ProdColumns t n con cols cs,
    c ~ 'Kind.Tree name '[] ('Kind.Prod (Con name) cs)
  ) => ConColumn t n con ('ConMeta name cols) c where
    conColumn con =
      Type.Tree (treePayload @t @n @a @meta undefined) (Type.Prod (prodColumns @t @n @con @cols con))

instance (
    meta ~ 'ColumnMeta cname rep d,
    Tree t n con meta c
  ) => ConColumn t n con ('ConMeta cname '[ 'ColumnMeta name rep d]) c where
    conColumn =
      tree @t @n @con @meta

class SumColumns (t :: Type) (n :: Type -> Type) (cons' :: Type) (cons :: [ConMeta]) (cs :: [Kind.Tree]) | cons t n -> cs where
  sumColumns :: cons' -> NP (Type.Tree t n) cs

instance SumColumns t n cons' '[] '[] where
  sumColumns _ =
    Nil

instance (
    TreeConElem t n cons' consTail con',
    ConColumn t n con' con c,
    SumColumns t n consTail cons cs
  ) => SumColumns t n cons' (con : cons) (c : cs) where
    sumColumns cons =
      conColumn @t @n @con' @con con :* sumColumns @t @n @consTail @cons conTail
      where
        (conTail, con) =
          treeConElem @t @n cons

class ADTColumn (t :: Type) (n :: Type -> Type) (d :: Type) (meta :: ADTMetadata) (eff :: [*]) (ct :: Type) (sub :: Kind.Node) | d meta eff ct t n -> sub where
  adtColumn :: d -> Type.Node t n sub

instance (
    TreeProduct t n d prod,
    ProdColumns t n prod cols cs
  ) => ADTColumn t n d ('ADTProd cols) '[] ct ('Kind.Prod d cs) where
  adtColumn d =
    Type.Prod (prodColumns @t @n @prod @cols (treeProduct @t @n d))

type SumIndexColumn e =
  'Kind.Tree ('NamedField "sum_index") e ('Kind.Prim Int)

instance (
    -- TreeImpl t n e,
    -- TreePrim f t n e indexMeta,
    TreeCons t n d cons,
    SumColumns t n cons cols cs,
    -- indexMeta ~ 'ColumnMeta ('NamedField "sum_index") (Rep '[Prim]) Int,
    node ~ 'Kind.Sum d cs
  ) => ADTColumn t n d ('ADTSum cols) '[] ct node where
  adtColumn d =
    Type.Sum (sumColumns @t @n @cons @cols (treeCons @t @n d))

class ColumnForKind (meta :: ColumnMeta) (t :: Type) (n :: Type -> Type) (a :: *) (eff :: [*]) (d :: Type) (ct :: Type) (sub :: Kind.Node) | eff d ct t n -> sub where
  columnForKind :: a -> Type.Node t n sub

instance (
    ADTColumn t n d meta effs ct sub
  ) => ColumnForKind _meta t n d (ADT meta rep : effs) d ct sub where
    columnForKind =
      adtColumn @t @n @d @meta @effs @ct

instance (
    TreePrim t n a meta,
    meta ~ 'ColumnMeta name rep d
  ) => ColumnForKind meta t n a '[] d ct ('Kind.Prim d) where
  columnForKind fa =
    Type.Prim (treePrim @t @n @a @meta fa)

instance {-# overlappable #-} (
    ColumnForKind meta t n a effs d ct sub
  ) => ColumnForKind meta t n a (eff : effs) d ct sub where
    columnForKind =
      columnForKind @meta @t @n @a @effs @d @ct

class Tree (t :: Type) (n :: Type -> Type) (a :: Type) (meta :: ColumnMeta) (tree :: Kind.Tree) | meta t n -> tree where
  tree :: a -> Type.Tree t n tree

instance (
    TreePayload t n a meta,
    ResolveTreeEffects rep d effs ct,
    meta ~ 'ColumnMeta name rep d,
    ColumnForKind meta t n a effs d ct sub,
    tree ~ 'Kind.Tree name '[] sub
  ) => Tree t n a meta tree where
    tree a =
      Type.Tree (treePayload @t @n @a @meta a) (columnForKind @meta @t @n @a @effs @d @ct a)

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

class Root (t :: Type) (n :: Type -> Type) (d :: Type) (c :: Kind.Tree) | d t n -> c where
  tableTree :: Type.Tree t n c

instance (
    TableMeta d meta,
    Tree t n d meta c
  ) => Root t n d c where
  tableTree =
    tree @t @n @d @meta undefined
