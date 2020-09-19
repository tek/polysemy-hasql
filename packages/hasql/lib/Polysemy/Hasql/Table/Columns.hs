{-# OPTIONS_GHC -Wno-unused-imports #-}
module Polysemy.Hasql.Table.Columns where

import qualified Data.Text as Text
import GHC.TypeLits (KnownSymbol)
import Generics.SOP (Code, Generic, fieldName)
import Generics.SOP.Type.Metadata (
  ConstructorInfo,
  ConstructorInfo(Record),
  DemoteFieldInfo,
  FieldInfo,
  demoteFieldInfo,
  )
import Prelude hiding (Enum, Generic)

import Polysemy.Db.Data.Column (Auto, Enum(Enum), Flatten, Prim, Sum)
import Polysemy.Db.Data.TableName (TableName(TableName))
import Polysemy.Db.Data.TableStructure (Column(Column), CompositeType(CompositeType), TableStructure(TableStructure))
import Polysemy.Db.SOP.Constraint (Ctors, DataName, IsRecord, dataName, dataSlugSymbol_)
import Polysemy.Hasql.Data.AlignColumns (AlignColumns)
import Polysemy.Hasql.Data.ColumnType (Auto', Auto'', AutoInternal, HeadRep, HeadRep2, TailRep, TailRep2)
import Polysemy.Hasql.Table.ColumnParams (
  ExplicitColumnParams(explicitColumnParams),
  ImplicitColumnParams(implicitColumnParams),
  )
import Polysemy.Hasql.Table.ColumnType (ColumnCode, ColumnType(columnType), EnumColumn, PrimColumn, SumColumn, SumRep)
import Polysemy.Hasql.Table.Identifier (dbIdentifier)
import Polysemy.Hasql.Table.Orphans ()
import Polysemy.Hasql.Table.TableName (GenTableName(genTableName))

type family RepCode a where
  RepCode Auto = Auto''
  RepCode a = Code a

columnName ::
  ∀ n t .
  DemoteFieldInfo n t =>
  Text
columnName =
  Text.dropWhile ('_' ==) (dbIdentifier (fieldName (demoteFieldInfo @_ @t (Proxy @n))))

class ColumnNames (ns :: [FieldInfo]) where
  columnNames :: NonEmpty Text

instance DemoteFieldInfo n d => ColumnNames '[n] where
  columnNames =
    pure (columnName @n @d)

instance (
    DemoteFieldInfo n d,
    ColumnNames (n1 : ns)
  ) => ColumnNames (n : n1 : ns) where
  columnNames =
    columnName @n @d :| toList (columnNames @(n1 : ns))

genColumnBasic ::
  ∀ r (d :: *) (n :: FieldInfo) .
  DemoteFieldInfo n d =>
  ExplicitColumnParams r =>
  ImplicitColumnParams d =>
  Text ->
  Maybe CompositeType ->
  Column
genColumnBasic colType composite =
  Column (columnName @n @d) colType par composite
  where
    par =
      explicitColumnParams @r <> implicitColumnParams @d

genColumnSum ::
  ∀ r (d :: *) (n :: FieldInfo) .
  DataName d =>
  DemoteFieldInfo n d =>
  GenCompositeType r d =>
  ExplicitColumnParams (Sum r) =>
  ImplicitColumnParams d =>
  Column
genColumnSum =
  genColumnBasic @(Sum r) @d @n (dbIdentifier (dataName @d)) (Just (genCompositeType @r @d))

class GenColumnPrim r n d where
  genColumnPrim :: Column

instance (
    DemoteFieldInfo n d,
    ColumnType d,
    ExplicitColumnParams r,
    ImplicitColumnParams d
  ) => GenColumnPrim r n d where
  genColumnPrim =
    genColumnBasic @r @d @n (columnType @d) Nothing

type family SumStyle dss :: * -> * where
  SumStyle '[] = Enum
  SumStyle ('[] : dss) = SumStyle dss
  SumStyle dss = Sum

class GenColumnAutoSum n d f where
  genColumnAutoSum :: Column

instance (
    DataName d,
    DemoteFieldInfo n d,
    GenCompositeType Auto d,
    ExplicitColumnParams (Sum Auto),
    ImplicitColumnParams d
  ) => GenColumnAutoSum n d Sum where
    genColumnAutoSum =
      genColumnSum @Auto @d @n

instance (
    GenColumnPrim Auto n d
  ) => GenColumnAutoSum n d Enum where
    genColumnAutoSum =
      genColumnPrim @Auto @n @d

class GenColumnAuto (n :: FieldInfo) d (dss :: [[*]]) where
  genColumnAuto :: Column

instance (
    GenColumnAutoSum n d (SumStyle (d1 : d2 : dss))
  ) => GenColumnAuto n d (d1 : d2 : dss) where
    genColumnAuto =
      genColumnAutoSum @n @d @(SumStyle (d1 : d2 : dss))

-- instance (
--     Generic d,
--     Code d ~ '[ds],
--     GenColumn (Prim Auto) n d
--   ) => GenColumnAuto n d '[ds] where
--     genColumnAuto =
--       genColumn @(Prim Auto) @n @d

-- instance {-# incoherent #-} (
--     Generic d,
--     Code d ~ ds,
--     GenColumn (Prim Auto) n d
--   ) => GenColumnAuto n d ds where
--     genColumnAuto =
--       genColumn @(Prim Auto) @n @d

class GenColumn (columnField :: FieldInfo) (columnType :: *) where
  genColumn :: Column

instance (
    DataName d,
    DemoteFieldInfo field d,
    GenCompositeType r d,
    ExplicitColumnParams (Sum r),
    ImplicitColumnParams d
  ) => GenColumn field (SumColumn r d dss) where
  genColumn =
    genColumnSum @r @d @field

instance (
    GenColumnPrim r field d
  ) => GenColumn field (PrimColumn r d) where
  genColumn =
    genColumnPrim @r @field @d

instance (
    GenColumnPrim r field d
  ) => GenColumn field (EnumColumn r d dss) where
  genColumn =
    genColumnPrim @r @field @d

class GenColumns' columnRepTypes (columnFields :: [FieldInfo]) (columnTypes :: [*]) where
  genColumns' :: [Column]

instance GenColumns' rep '[] '[] where
    genColumns' =
      mempty

instance (
    GenColumn n (ColumnCode Auto d),
    GenColumns' Auto ns ds
  ) => GenColumns' Auto (n : ns) (d : ds) where
    genColumns' =
      pure (genColumn @n @(ColumnCode Auto d)) <> genColumns' @Auto @ns @ds

instance {-# overlappable #-} (
    GenColumn n (ColumnCode (HeadRep reps) d),
    GenColumns' (TailRep reps) ns ds
  ) => GenColumns' reps (n : ns) (d : ds) where
    genColumns' =
      genColumn @n @(ColumnCode (HeadRep reps) d) : genColumns' @(TailRep reps) @ns @ds

instance (
    GenColumns rep d,
    GenColumns' reps ns ds
  ) => GenColumns' (Flatten rep : reps) (n : ns) (d : ds) where
    genColumns' =
      genColumns @rep @d <> genColumns' @reps @ns @ds

class GenColumns (rowRepType :: *) (rowRecordType :: *) where
  genColumns :: [Column]

instance {-# overlappable #-} (
    IsRecord d ds name ns,
    IsRecord rep reps rname rns,
    AlignColumns rep d,
    GenColumns' reps ns ds
  ) => GenColumns rep d where
    genColumns =
      genColumns' @reps @ns @ds

instance (
    IsRecord d ds name ns,
    GenColumns' Auto' ns ds
  ) => GenColumns Auto d where
    genColumns =
      genColumns' @Auto' @ns @ds

class GenCtorType columnRepTypes (columnTypes :: [*]) (dataCtor :: ConstructorInfo) where
  genCtorType :: TableStructure

instance (
    GenColumns' rep fs ts,
    d ~ 'Record n fs,
    KnownSymbol n
  ) => GenCtorType rep ts d where
  genCtorType =
    TableStructure (TableName (dataSlugSymbol_ @n)) (genColumns' @rep @fs @ts)

class GenSumCtorsColumns repTypes (columnTypes :: [[*]]) (dataCtors :: [ConstructorInfo]) where
  genSumCtorsColumns :: [TableStructure]

instance GenSumCtorsColumns rep '[] '[] where
  genSumCtorsColumns =
    mempty

instance (
    GenCtorType (HeadRep2 (r : rep)) t c,
    GenSumCtorsColumns (TailRep2 (r : rep)) ts cs
  ) => GenSumCtorsColumns (r : rep) (t : ts) (c : cs) where
  genSumCtorsColumns =
    pure (genCtorType @(HeadRep2 (r : rep)) @t @c) <> genSumCtorsColumns @(TailRep2 (r : rep)) @ts @cs

class GenSumColumns (sumRepType :: *) (sumType :: *) where
  genSumColumns :: [TableStructure]

instance (
    Ctors d ctors dTypes,
    GenSumCtorsColumns (RepCode rep) dTypes ctors
  ) => GenSumColumns rep d where
    genSumColumns =
      genSumCtorsColumns @(RepCode rep) @dTypes @ctors

class GenCompositeType (rep :: *) (d :: *) where
  genCompositeType :: CompositeType

instance (GenTableName d, GenSumColumns rep d) => GenCompositeType rep d where
  genCompositeType =
    CompositeType (genTableName @d) (Column "sum_index" "integer" def Nothing) (genSumColumns @rep @d)
