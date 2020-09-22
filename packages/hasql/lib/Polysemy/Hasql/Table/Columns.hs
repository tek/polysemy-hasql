module Polysemy.Hasql.Table.Columns where

import qualified Data.Text as Text
import GHC.TypeLits (KnownSymbol)
import Generics.SOP (fieldName)
import Generics.SOP.Type.Metadata (
  ConstructorInfo(Record),
  DemoteFieldInfo,
  FieldInfo,
  demoteFieldInfo,
  )

import Polysemy.Db.Data.Column (Flatten, Sum)
import Polysemy.Db.Data.TableName (TableName(TableName))
import Polysemy.Db.Data.TableStructure (Column(Column), CompositeType(CompositeType), TableStructure(TableStructure))
import Polysemy.Db.SOP.Constraint (Ctors, DataName, IsRecord, dataName, dataSlugSymbol_)
import Polysemy.Hasql.Table.ColumnParams (
  ExplicitColumnParams(explicitColumnParams),
  ImplicitColumnParams(implicitColumnParams),
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

type BasicColumn r field d =
  (DemoteFieldInfo field d, ExplicitColumnParams r, ImplicitColumnParams d)

genColumnBasic ::
  ∀ r (field :: FieldInfo) (d :: *) .
  BasicColumn r field d =>
  Text ->
  Maybe CompositeType ->
  Column
genColumnBasic colType composite =
  Column (columnName @field @d) colType par composite
  where
    par =
      explicitColumnParams @r <> implicitColumnParams @d

genColumnSum ::
  ∀ r (field :: FieldInfo) (d :: *) .
  DataName d =>
  GenCompositeType r d =>
  BasicColumn (Sum r) field d =>
  Column
genColumnSum =
  genColumnBasic @(Sum r) @field @d (dbIdentifier (dataName @d)) (Just (genCompositeType @r @d))

genColumnPrim ::
  ∀ r (field :: FieldInfo) d .
  BasicColumn r field d =>
  ColumnType d =>
  Column
genColumnPrim =
  genColumnBasic @r @field @d (columnType @d) Nothing

class GenColumn (columnField :: FieldInfo) (columnRep :: *) (columnType :: *) where
  genColumn :: Column

instance {-# overlappable #-} (
    BasicColumn r field d,
    ColumnType d
  ) => GenColumn field r d where
  genColumn =
    genColumnPrim @r @field @d

instance (
    DataName d,
    GenCompositeType rep d,
    BasicColumn (Sum rep) field d
  ) => GenColumn field (Sum (SumColumn rep)) d where
  genColumn =
    genColumnSum @rep @field @d

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
    TableStructure (TableName (dataSlugSymbol_ @n)) (genProdColumns @rep @fs @ts)

class GenSumCtorsColumns (repTypes :: [*]) (columnTypes :: [[*]]) (dataCtors :: [ConstructorInfo]) where
  genSumCtorsColumns :: [TableStructure]

instance GenSumCtorsColumns rep '[] '[] where
  genSumCtorsColumns =
    mempty

instance (
    GenCtorType r t c,
    GenSumCtorsColumns reps ts cs
  ) => GenSumCtorsColumns (ProdColumn r : reps) (t : ts) (c : cs) where
  genSumCtorsColumns =
    pure (genCtorType @r @t @c) <> genSumCtorsColumns @reps @ts @cs

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
