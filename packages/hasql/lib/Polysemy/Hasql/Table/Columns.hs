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
import Prelude hiding (Generic, Enum)

import Polysemy.Db.Data.Column (Auto, Enum(Enum), Flatten, Prim, Sum)
import Polysemy.Db.Data.TableName (TableName(TableName))
import Polysemy.Db.Data.TableStructure (Column(Column), CompositeType(CompositeType), TableStructure(TableStructure))
import Polysemy.Db.SOP.Constraint (Ctors, DataName, IsRecord, dataName, dataSlugSymbol_)
import Polysemy.Hasql.Data.AlignColumns (AlignColumns)
import Polysemy.Hasql.Data.ColumnType (Auto', AutoInternal, HeadRep, TailRep)
import Polysemy.Hasql.Table.ColumnParams (
  ExplicitColumnParams(explicitColumnParams),
  ImplicitColumnParams(implicitColumnParams),
  )
import Polysemy.Hasql.Table.ColumnType (ColumnType(columnType))
import Polysemy.Hasql.Table.Identifier (dbIdentifier)
import Polysemy.Hasql.Table.Orphans ()
import Polysemy.Hasql.Table.TableName (GenTableName(genTableName))

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

instance (
    Generic d,
    Code d ~ '[ds],
    GenColumn (Prim Auto) n d
  ) => GenColumnAuto n d '[ds] where
    genColumnAuto =
      genColumn @(Prim Auto) @n @d

instance {-# incoherent #-} (
    Generic d,
    Code d ~ ds,
    GenColumn (Prim Auto) n d
  ) => GenColumnAuto n d ds where
    genColumnAuto =
      genColumn @(Prim Auto) @n @d

class GenColumn (columnRepType :: *) (columnField :: FieldInfo) (columnType :: *) where
  genColumn :: Column

instance {-# incoherent #-} (
    Generic d,
    GenColumnAuto n d (Code d)
  ) => GenColumn AutoInternal n d where
    genColumn =
      genColumnAuto @n @d @(Code d)

instance {-# incoherent #-} (
    Generic d,
    GenColumnAuto n d (Code d)
  ) => GenColumn Auto n d where
    genColumn =
      genColumnAuto @n @d @(Code d)

instance {-# incoherent #-} (
    GenColumnPrim (Prim r) n d
  ) => GenColumn (Prim r) n d where
  genColumn =
    genColumnPrim @(Prim r) @n @d

instance (
    DataName d,
    DemoteFieldInfo n d,
    GenCompositeType r d,
    ExplicitColumnParams (Sum r),
    ImplicitColumnParams d
  ) => GenColumn (Sum r) n d where
  genColumn =
    genColumnSum @r @d @n

instance GenColumnPrim r n Int => GenColumn r n Int where
  genColumn =
    genColumnPrim @r @n @Int

instance GenColumnPrim r n Float => GenColumn r n Float where
  genColumn =
    genColumnPrim @r @n @Float

instance GenColumnPrim r n Double => GenColumn r n Double where
  genColumn =
    genColumnPrim @r @n @Double

instance GenColumnPrim r n Text => GenColumn r n Text where
  genColumn =
    genColumnPrim @r @n @Text

instance GenColumnPrim r n UUID => GenColumn r n UUID where
  genColumn =
    genColumnPrim @r @n @UUID

class GenColumns' columnRepTypes (columnFields :: [FieldInfo]) (columnTypes :: [*]) where
  genColumns' :: NonEmpty Column

instance {-# overlappable #-} (
    GenColumn r n d
  ) => GenColumns' '[r] '[n] '[d] where
    genColumns' =
      pure (genColumn @r @n @d)

instance (
    GenColumn Auto n d
  ) => GenColumns' Auto '[n] '[d] where
    genColumns' =
      pure (genColumn @Auto @n @d)

instance (
    GenColumn Auto n d,
    GenColumns' Auto (n1 : ns) (d1 : ds)
  ) => GenColumns' Auto (n : n1 : ns) (d : d1 : ds) where
    genColumns' =
      pure (genColumn @Auto @n @d) <> genColumns' @Auto @(n1 : ns) @(d1 : ds)

instance GenColumns rep d => GenColumns' '[Flatten rep] '[n] '[d] where
    genColumns' =
      genColumns @rep @d

instance {-# overlappable #-} (
    GenColumn (HeadRep reps) n d,
    GenColumns' (TailRep reps) (n1 : ns) (d1 : ds)
  ) => GenColumns' reps (n : n1 : ns) (d : d1 : ds) where
    genColumns' =
      genColumn @(HeadRep reps) @n @d :| toList (genColumns' @(TailRep reps) @(n1 : ns) @(d1 : ds))

instance (
    GenColumns rep d,
    GenColumns' (r1 : reps) (n1 : ns) (d1 : ds)
  ) => GenColumns' (Flatten rep : r1 : reps) (n : n1 : ns) (d : d1 : ds) where
    genColumns' =
      genColumns @rep @d <> genColumns' @(r1 : reps) @(n1 : ns) @(d1 : ds)

class GenColumns (rowRepType :: *) (rowRecordType :: *) where
  genColumns :: NonEmpty Column

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
  genSumCtorsColumns :: NonEmpty TableStructure

instance GenCtorType rep t c => GenSumCtorsColumns '[rep] '[t] '[c] where
  genSumCtorsColumns =
    pure (genCtorType @rep @t @c)

instance GenCtorType Auto t c => GenSumCtorsColumns Auto '[t] '[c] where
  genSumCtorsColumns =
    pure (genCtorType @Auto @t @c)

instance (
    GenCtorType r t c,
    GenSumCtorsColumns rep ts (c1 : cs)
  ) => GenSumCtorsColumns (r : rep) (t : ts) (c : c1 : cs) where
  genSumCtorsColumns =
    pure (genCtorType @r @t @c) <> genSumCtorsColumns @rep @ts @(c1 : cs)

instance (
    GenCtorType Auto t c,
    GenSumCtorsColumns Auto ts (c1 : cs)
  ) => GenSumCtorsColumns Auto (t : ts) (c : c1 : cs) where
  genSumCtorsColumns =
    pure (genCtorType @Auto @t @c) <> genSumCtorsColumns @Auto @ts @(c1 : cs)

class GenSumColumns (sumRepType :: *) (sumType :: *) where
  genSumColumns :: NonEmpty TableStructure

instance {-# overlappable #-} (
    Ctors rep rCtors rTypes,
    Ctors d ctors dTypes,
    GenSumCtorsColumns rTypes dTypes ctors
  ) => GenSumColumns rep d where
    genSumColumns =
      genSumCtorsColumns @rTypes @dTypes @ctors

instance (
    Ctors d ctors dTypes,
    GenSumCtorsColumns Auto dTypes ctors
  ) => GenSumColumns Auto d where
    genSumColumns =
      genSumCtorsColumns @Auto @dTypes @ctors

class GenCompositeType (rep :: *) (d :: *) where
  genCompositeType :: CompositeType

instance (GenTableName d, GenSumColumns rep d) => GenCompositeType rep d where
  genCompositeType =
    CompositeType (genTableName @d) (Column "sum_index" "integer" def Nothing) (genSumColumns @rep @d)
