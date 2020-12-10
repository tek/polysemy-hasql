module Polysemy.Hasql.Table.Columns where

import qualified Data.Text as Text
import Generics.SOP (fieldName)
import Generics.SOP.Type.Metadata (
  ConstructorInfo(Record),
  DemoteFieldInfo,
  FieldInfo,
  demoteFieldInfo,
  )
import Polysemy.Db.Data.Column (Json)

import Polysemy.Db.Data.Column (Flatten, Sum)
import Polysemy.Db.Data.TableName (TableName(TableName))
import Polysemy.Db.Data.TableStructure (Column(Column), CompositeType(CompositeType), TableStructure(TableStructure))
import Polysemy.Db.SOP.Constraint (Ctors, DataName, IsRecord, dataName, slugSymbol_)
import Polysemy.Hasql.Table.ColumnOptions (
  ExplicitColumnOptions(explicitColumnOptions),
  ImplicitColumnOptions(implicitColumnOptions),
  )
import Polysemy.Hasql.Table.ColumnType (ColumnType(columnType))
import Polysemy.Hasql.Table.Identifier (dbIdentifier)
import Polysemy.Hasql.Table.Representation (ProdColumn, ReifyRepTable, SumColumn)
import Polysemy.Hasql.Table.TableName (GenTableName(genTableName))

sumIndexColumn :: Column
sumIndexColumn =
  Column "sum_index" "integer" def Nothing

columnName ::
  ∀ n t .
  DemoteFieldInfo n t =>
  Text
columnName =
  Text.dropWhile ('_' ==) (dbIdentifier (fieldName (demoteFieldInfo @_ @t (Proxy @n))))

class ColumnNames (fields :: [FieldInfo]) where
  columnNames :: [Text]

instance ColumnNames '[] where
  columnNames =
    mempty

instance (
    DemoteFieldInfo field d,
    ColumnNames fields
  ) => ColumnNames (field : fields) where
  columnNames =
    columnName @field @d : columnNames @fields

type BasicColumn rep field d =
  (DemoteFieldInfo field d, ExplicitColumnOptions rep, ImplicitColumnOptions d)

genColumnBasic ::
  ∀ rep (field :: FieldInfo) (d :: *) .
  BasicColumn rep field d =>
  Text ->
  Maybe CompositeType ->
  Column
genColumnBasic colType composite =
  Column (columnName @field @d) colType par composite
  where
    par =
      explicitColumnOptions @rep <> implicitColumnOptions @d

genColumnSum ::
  ∀ rep sum (field :: FieldInfo) (d :: *) .
  DataName d =>
  GenCompositeType sum d =>
  BasicColumn rep field d =>
  Column
genColumnSum =
  genColumnBasic @rep @field @d (dbIdentifier (dataName @d)) (Just (genCompositeType @sum @d))

genColumnPrim ::
  ∀ rep (field :: FieldInfo) d .
  BasicColumn rep field d =>
  ColumnType d =>
  Column
genColumnPrim =
  genColumnBasic @rep @field @d (columnType @d) Nothing

class GenColumn (columnField :: FieldInfo) (columnRep :: *) (columnType :: *) where
  genColumn :: Column

instance {-# overlappable #-} (
    BasicColumn rep field d,
    ColumnType d
  ) => GenColumn field rep d where
  genColumn =
    genColumnPrim @rep @field @d

instance (
    BasicColumn rep field d
  ) => GenColumn field (Json rep) d where
  genColumn =
    genColumnBasic @rep @field @d "json" Nothing

instance (
    DataName d,
    GenCompositeType sumRep d,
    BasicColumn rep field d
  ) => GenColumn field (Sum rep (SumColumn sumRep)) d where
  genColumn =
    genColumnSum @rep @sumRep @field @d

class GenProdColumns (columnRepTypes :: [*]) (columnFields :: [FieldInfo]) (columnTypes :: [*]) where
  genProdColumns :: [Column]

instance GenProdColumns rep '[] '[] where
    genProdColumns =
      mempty

instance {-# overlappable #-} (
    GenColumn field rep d,
    GenProdColumns reps fields ds
  ) => GenProdColumns (rep : reps) (field : fields) (d : ds) where
    genProdColumns =
      genColumn @field @rep @d : genProdColumns @reps @fields @ds

instance (
    Columns rep d,
    GenProdColumns reps ns ds
  ) => GenProdColumns (Flatten rep : reps) (field : ns) (d : ds) where
    genProdColumns =
      columns @rep @d <> genProdColumns @reps @ns @ds

class GenCtorType columnRepTypes (columnTypes :: [*]) (dataCtor :: ConstructorInfo) where
  genCtorType :: TableStructure

instance (
    GenProdColumns rep fs ts,
    d ~ 'Record n fs,
    KnownSymbol n
  ) => GenCtorType rep ts d where
  genCtorType =
    TableStructure (TableName (slugSymbol_ @n)) (genProdColumns @rep @fs @ts)

class GenSumCtorsColumns (repTypes :: [*]) (columnTypes :: [[*]]) (dataCtors :: [ConstructorInfo]) where
  genSumCtorsColumns :: [TableStructure]

instance GenSumCtorsColumns rep '[] '[] where
  genSumCtorsColumns =
    mempty

instance (
    GenCtorType rep t c,
    GenSumCtorsColumns reps ts cs
  ) => GenSumCtorsColumns (ProdColumn rep : reps) (t : ts) (c : cs) where
  genSumCtorsColumns =
    pure (genCtorType @rep @t @c) <> genSumCtorsColumns @reps @ts @cs

class GenSumColumns (sumRepType :: [*]) (sumType :: *) where
  genSumColumns :: [TableStructure]

instance (
    Ctors d ctors dTypes,
    GenSumCtorsColumns rep dTypes ctors
  ) => GenSumColumns rep d where
    genSumColumns =
      genSumCtorsColumns @rep @dTypes @ctors

class GenCompositeType (rep :: [*]) (d :: *) where
  genCompositeType :: CompositeType

instance (GenTableName d, GenSumColumns rep d) => GenCompositeType rep d where
  genCompositeType =
    CompositeType (genTableName @d) sumIndexColumn (genSumColumns @rep @d)

class GenColumns (rep :: *) (ns :: [FieldInfo]) (ds :: [*]) where
  genColumns :: [Column]

instance (
    GenProdColumns rep ns ds
  ) => GenColumns (ProdColumn rep) ns ds where
  genColumns =
    genProdColumns @rep @ns @ds

class Columns (rowRepType :: *) (rowRecordType :: *) where
  columns :: [Column]

instance (
    IsRecord d ds name ns,
    GenColumns (ReifyRepTable rep d) ns ds
  ) => Columns rep d where
    columns =
      genColumns @(ReifyRepTable rep d) @ns @ds
