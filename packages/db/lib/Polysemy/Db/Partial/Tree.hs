module Polysemy.Db.Partial.Tree where

import Generics.SOP (All, NP(Nil, (:*)), hcmap)

import Polysemy.Db.Data.Column (Auto, Con, Product, Rep)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Data.PartialField as PartialField
import Polysemy.Db.Data.PartialField (FieldPath (FieldName), FieldUpdate(FieldUpdate), PartialField)
import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (DataName, symbolText)
import Polysemy.Db.Text.DbIdentifier (quotedDbId)
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Effect (ResolveTreeEffects)
import Polysemy.Db.Tree.Meta (ADTMetadata (ADTProd, ADTSum), ColumnMeta(ColumnMeta), ConMeta(ConMeta))
import qualified Polysemy.Db.Type.Data.Tree as Type

type PartialTree = Type.Tree () PartialField
type PartialNode = Type.Node () PartialField

class ProdColumns (metas :: [ColumnMeta]) (cs :: [Kind.Tree]) | metas -> cs where
  prodColumns :: NP PartialTree cs

instance ProdColumns '[] '[] where
  prodColumns =
    Nil

instance (
    Tree meta c,
    ProdColumns metas cs
  ) => ProdColumns (meta : metas) (c : cs) where
    prodColumns =
      column @meta :* prodColumns @metas

class ConColumn (meta :: ConMeta) (c :: Kind.Tree) | meta -> c where
  conColumn :: PartialTree c

instance {-# overlappable #-} (
    ProdColumns cols cs,
    c ~ 'Kind.Tree name '[] ('Kind.Prod (Con name) cs)
  ) => ConColumn ('ConMeta name cols) c where
    conColumn =
      Type.Tree () (Type.Prod (prodColumns @cols))

instance (
    meta ~ 'ColumnMeta cname rep d,
    Tree meta c
  ) => ConColumn ('ConMeta cname '[ 'ColumnMeta name rep d]) c where
    conColumn =
      column @meta

class SumColumns (cons :: [ConMeta]) (cs :: [Kind.Tree]) | cons -> cs where
  sumColumns :: NP PartialTree cs

instance SumColumns '[] '[] where
  sumColumns =
    Nil

instance (
    ConColumn con c,
    SumColumns cons cs
  ) => SumColumns (con : cons) (c : cs) where
    sumColumns =
      conColumn @con :* sumColumns @cons

class ADTColumn (d :: *) (meta :: ADTMetadata) (eff :: [*]) (ct :: *) (t :: Kind.Node) | d meta eff ct -> t where
  adtColumn :: PartialNode t

instance (
    ProdColumns cols cs
  ) => ADTColumn d ('ADTProd cols) '[] ct ('Kind.Prod d cs) where
  adtColumn =
    Type.Prod (prodColumns @cols)

type SumIndexColumn =
  'Kind.Tree ('NamedField "sum_index") '[] ('Kind.Prim Int)

indexColumn :: PartialTree SumIndexColumn
indexColumn =
  Type.Tree () (Type.Prim PartialField.Keep)

instance (
    SumColumns cols cs
  ) => ADTColumn d ('ADTSum cols) '[] ct ('Kind.Sum d (SumIndexColumn : cs)) where
  adtColumn =
    Type.Sum (indexColumn :* sumColumns @cols)

class ColumnForKind (eff :: [*]) (d :: *) (ct :: *) (t :: Kind.Node) | eff d ct -> t where
  columnForKind :: PartialNode t

instance (
    ADTColumn d meta effs ct t
  ) => ColumnForKind (ADT meta rep : effs) d ct t where
    columnForKind =
      adtColumn @d @meta @effs @ct

instance ColumnForKind '[] d ct ('Kind.Prim d) where
  columnForKind =
    Type.Prim PartialField.Keep

instance {-# overlappable #-} (
    ColumnForKind effs d ct t
  ) => ColumnForKind (eff : effs) d ct t where
    columnForKind =
      columnForKind @effs @d @ct

class Tree (meta :: ColumnMeta) (c :: Kind.Tree) | meta -> c where
  column :: PartialTree c

instance (
    ResolveTreeEffects rep d effs t,
    ColumnForKind effs d t c
  ) => Tree ('ColumnMeta name rep d) ('Kind.Tree name '[] c) where
    column =
      Type.Tree () (columnForKind @effs @d @t)

class KnownSymbol name => TableName (d :: *) (name :: Symbol) | d -> name where
  tableName :: Text
  tableName =
    quotedDbId (symbolText @name)

instance {-# overlappable #-} (
    KnownSymbol name,
    DataName d name
  ) => TableName d name

instance TableName d name => TableName (Uid i d) name where

class TableMeta (d :: *) (meta :: ColumnMeta) | d -> meta

instance {-# overlappable #-} (
    TableName d name,
    meta ~ 'ColumnMeta ('NamedField name) (Rep '[Product Auto]) d
  ) => TableMeta d meta

class Root (d :: *) (c :: Kind.Tree) | d -> c where
  tableTree :: PartialTree c

instance (
    TableMeta d meta,
    Tree meta c
  ) => Root d c where
  tableTree =
    column @meta

partialTree ::
  ∀ d t .
  Root d t =>
  PartialTree t
partialTree =
  tableTree @d

class InsertFieldNode (path :: FieldPath) (a :: *) (n :: Kind.Node) where
  insertFieldNode :: FieldUpdate path a -> PartialNode n -> PartialNode n

instance (
    All (InsertFieldTree path a) ts
  ) => InsertFieldNode path a ('Kind.Prod t ts) where
    insertFieldNode update (Type.Prod sub) =
      Type.Prod (hcmap (Proxy @(InsertFieldTree path a)) (insertFieldTree update) sub)

instance (
    All (InsertFieldTree path a) ts
  ) => InsertFieldNode path a ('Kind.Sum t ts) where
    insertFieldNode update (Type.Sum sub) =
      Type.Sum (hcmap (Proxy @(InsertFieldTree path a)) (insertFieldTree update) sub)

class InsertFieldTree (path :: FieldPath) (a :: *) (t :: Kind.Tree) where
  insertFieldTree :: FieldUpdate path a -> PartialTree t -> PartialTree t

instance (
    KnownSymbol name
  ) => InsertFieldTree ('FieldName name) a ('Kind.Tree ('NamedField name) '[] ('Kind.Prim a)) where
  insertFieldTree (FieldUpdate a) (Type.Tree t _) =
    Type.Tree t (Type.Prim (PartialField.Update (symbolText @name) a))

instance {-# overlappable #-} InsertFieldTree ('FieldName name) a ('Kind.Tree ('NamedField _name) '[] _n) where
  insertFieldTree _ =
    id

class InsertField (path :: FieldPath) (a :: *) (t :: Kind.Tree) where
  insertField :: FieldUpdate path a -> PartialTree t -> PartialTree t

instance (
    InsertFieldNode path a n
  ) => InsertField path a ('Kind.Tree ('NamedField _name) '[] n) where
  insertField update (Type.Tree t n) =
    Type.Tree t (insertFieldNode @path @a @n update n)

(..>) ::
  InsertField path a t =>
  PartialTree t ->
  FieldUpdate path a ->
  PartialTree t
(..>) =
  flip insertField

class UpdatePartialTree (t :: Kind.Tree) where

updatePartialTree ::
  PartialTree t ->
  d ->
  d
updatePartialTree =
  undefined
