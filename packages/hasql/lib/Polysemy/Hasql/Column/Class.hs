module Polysemy.Hasql.Column.Class where

import Generics.SOP (NP(Nil, (:*)))
import Polysemy.Db.Data.Column (Con, Prim, PrimQuery, Rep)
import Polysemy.Db.Data.FieldId (FieldId(NamedField), FieldIdText, fieldIdText)
import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (DataName, symbolText)
import Polysemy.Db.Text.DbIdentifier (quotedDbId)
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Meta (
  ADTMetadata(ADTSum, ADTProd),
  ColumnMeta(ColumnMeta),
  ConMeta(ConMeta),
  ProdDefaultRep,
  )
import qualified Polysemy.Db.Type.Data.Tree as Type
import Polysemy.Db.Type.Data.Tree (ColumnData(ColumnData))

import Polysemy.Hasql.Column.Effect (ResolveColumnEffects)
import Polysemy.Hasql.ColumnType (EffectfulColumnType, effectfulColumnType)
import Polysemy.Hasql.Table.ColumnOptions (
  ImplicitColumnOptions,
  RepOptions(repOptions),
  RepToList,
  implicitColumnOptions,
  )

class ColumnWithOptions (rep :: *) (d :: *) where
  columnWithOptions :: Text -> Type.DbType col -> Type.Column ('Kind.Tree name eff col)

instance (
    ImplicitColumnOptions d,
    RepOptions (RepToList rep)
  ) => ColumnWithOptions rep d where
    columnWithOptions tpe =
      Type.Tree (ColumnData tpe options)
      where
        options =
          repOptions @(RepToList rep) <> implicitColumnOptions @d

class ProdColumns (metas :: [ColumnMeta]) (cs :: [Kind.Tree [*]]) | metas -> cs where
  prodColumns :: NP Type.Column cs

instance ProdColumns '[] '[] where
  prodColumns =
    Nil

instance (
    Column meta c,
    ProdColumns metas cs
  ) => ProdColumns (meta : metas) (c : cs) where
    prodColumns =
      column @meta :* prodColumns @metas

class ConColumn (meta :: ConMeta) (c :: Kind.Tree [*]) | meta -> c where
  conColumn :: Type.Column c

instance {-# overlappable #-} (
    FieldIdText name,
    ProdColumns cols cs,
    c ~ 'Kind.Tree name '[] ('Kind.Prod (Con name) cs)
  ) => ConColumn ('ConMeta name cols) c where
    conColumn =
      Type.Tree (ColumnData (fieldIdText @name) def) (Type.Prod (prodColumns @cols))

instance (
    meta ~ 'ColumnMeta cname rep d,
    Column meta c
  ) => ConColumn ('ConMeta cname '[ 'ColumnMeta name rep d]) c where
    conColumn =
      column @meta

class SumColumns (cons :: [ConMeta]) (cs :: [Kind.Tree [*]]) | cons -> cs where
  sumColumns :: NP Type.Column cs

instance SumColumns '[] '[] where
  sumColumns =
    Nil

instance (
    ConColumn con c,
    SumColumns cons cs
  ) => SumColumns (con : cons) (c : cs) where
    sumColumns =
      conColumn @con :* sumColumns @cons

class ADTColumn (d :: *) (meta :: ADTMetadata) (eff :: [*]) (ct :: *) (t :: Kind.Node [*]) | d meta eff ct -> t where
  adtColumn :: Type.DbType t

instance (
    ProdColumns cols cs
  ) => ADTColumn d ('ADTProd cols) '[] ct ('Kind.Prod d cs) where
  adtColumn =
    Type.Prod (prodColumns @cols)

type SumIndexColumn =
  'Kind.Tree ('NamedField "sum_index") '[Prim] ('Kind.Prim Int)

indexColumn :: Type.Column SumIndexColumn
indexColumn =
  Type.Tree (ColumnData "bigint" def) (Type.Prim Proxy)

instance (
    SumColumns cols cs
  ) => ADTColumn d ('ADTSum cols) '[] ct ('Kind.Sum d (SumIndexColumn : cs)) where
  adtColumn =
    Type.Sum (indexColumn :* sumColumns @cols)

class ColumnForKind (eff :: [*]) (d :: *) (ct :: *) (t :: Kind.Node [*]) | eff d ct -> t where
  columnForKind :: Type.DbType t

instance (
    ADTColumn d meta effs ct t
  ) => ColumnForKind (ADT meta rep : effs) d ct t where
    columnForKind =
      adtColumn @d @meta @effs @ct

instance ColumnForKind '[] d ct ('Kind.Prim d) where
  columnForKind =
    Type.Prim Proxy

instance {-# overlappable #-} (
    ColumnForKind effs d ct t
  ) => ColumnForKind (eff : effs) d ct t where
    columnForKind =
      columnForKind @effs @d @ct

class Column (meta :: ColumnMeta) (c :: Kind.Tree [*]) | meta -> c where
  column :: Type.Column c

instance (
    ResolveColumnEffects rep d effs t,
    ColumnForKind effs d t c,
    EffectfulColumnType effs d,
    ColumnWithOptions rep d
  ) => Column ('ColumnMeta name rep d) ('Kind.Tree name effs c) where
    column =
      columnWithOptions @rep @d (effectfulColumnType @effs @d) (columnForKind @effs @d @t)

class KnownSymbol name => TableName (d :: *) (name :: Symbol) | d -> name where
  tableName :: Text
  tableName =
    quotedDbId (symbolText @name)

instance {-# overlappable #-} (
    KnownSymbol name,
    DataName d name
  ) => TableName d name

instance TableName d name => TableName (Uid i d) name where

class TableMeta (rep :: *) (d :: *) (meta :: ColumnMeta) | rep d -> meta

instance {-# overlappable #-} (
    TableName d name,
    meta ~ 'ColumnMeta ('NamedField name) (ProdDefaultRep rep) d
  ) => TableMeta rep d meta

instance TableMeta (PrimQuery name) d ('ColumnMeta ('NamedField name) (Rep '[Prim]) d)

class TableColumn (rep :: *) (d :: *) (c :: Kind.Tree [*]) | rep d -> c where
  tableColumn :: Type.Column c

instance (
    TableMeta rep d meta,
    Column meta c
  ) => TableColumn rep d c where
  tableColumn =
    column @meta
