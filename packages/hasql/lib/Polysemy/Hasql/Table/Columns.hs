{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Polysemy.Hasql.Table.Columns where

import qualified Data.Text as Text
import GHC.TypeLits (AppendSymbol, Symbol)
import Generics.SOP hiding (FieldInfo)
import Generics.SOP.Type.Metadata (DemoteFieldInfo, FieldInfo(FieldInfo), demoteFieldInfo)
import Prelude hiding (Generic)
import Type.Errors (ErrorMessage(Text, ShowType), TypeError)
import Type.Errors.Pretty (type (<>))

import Polysemy.Db.Data.Column (Auto, Flatten, ForeignKey, Prim, PrimaryKey, Unique)
import Polysemy.Db.Data.ColumnParams (ColumnParams(..))
import Polysemy.Db.Data.Columns (Column(Column))
import Polysemy.Hasql.Table.ColumnType (ColumnType(columnType))
import Polysemy.Hasql.Table.Identifier (dbIdentifier)
import Polysemy.Db.SOP.Constraint (IsRecord)

class ExplicitColumnParams r where
  explicitColumnParams :: ColumnParams

instance ExplicitColumnParams (Prim Unique) where
  explicitColumnParams =
    def { unique = True }

instance ExplicitColumnParams (Prim PrimaryKey) where
  explicitColumnParams =
    def { primaryKey = True }

instance ExplicitColumnParams (Prim Auto) where
  explicitColumnParams =
    def

instance ExplicitColumnParams Auto where
  explicitColumnParams =
    def

instance ExplicitColumnParams AutoInternal where
  explicitColumnParams =
    def

instance ExplicitColumnParams (Prim ForeignKey) where
  explicitColumnParams =
    def

class ImplicitColumnParams d where
  implicitColumnParams :: ColumnParams

instance {-# OVERLAPPABLE #-} ImplicitColumnParams d where
  implicitColumnParams =
    def

instance ImplicitColumnParams d => ImplicitColumnParams (Maybe d) where
  implicitColumnParams =
    implicitColumnParams @d <> def { notNull = False }

columnName ::
  ∀ n t .
  DemoteFieldInfo n t =>
  Text
columnName =
  Text.dropWhile ('_' ==) (dbIdentifier (fieldName (demoteFieldInfo @n @t (Proxy @n))))

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

class GenColumn r t d where
  genColumn :: Column

instance (
    DemoteFieldInfo n d,
    ColumnType d,
    ExplicitColumnParams r,
    ImplicitColumnParams d
  ) => GenColumn r n d where
  genColumn =
    Column (columnName @n @d) tpe par
    where
      par =
        explicitColumnParams @r <> implicitColumnParams @d
      tpe =
        columnType @d

data AutoInternal =
  AutoInternal
  deriving (Eq, Show)

type Auto' = '[AutoInternal]

class GenColumns' reps ns ds where
  genColumns' :: NonEmpty Column

instance {-# OVERLAPPABLE #-} (
    GenColumn r n d
  ) => GenColumns' '[r] '[n] '[d] where
    genColumns' =
      pure (genColumn @r @n @d)

instance GenColumns rep d => GenColumns' '[Flatten rep] '[n] '[d] where
    genColumns' =
      genColumns @rep @d

type family HeadRep rep where
  HeadRep Auto' = Auto
  HeadRep (r : r1 : reps) = r

type family TailRep rep where
  TailRep Auto' = Auto'
  TailRep (r : r1 : reps) = r1 : reps
  TailRep '[r] = TypeError ('Text "too few types in rep for GenColumns after '" <> 'ShowType r <> "'")

instance {-# OVERLAPPABLE #-} (
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

class GenColumns rep d where
  genColumns :: NonEmpty Column

type family SameName_ d (n :: Symbol) (rn_ :: Symbol) (rn :: Symbol) :: Constraint where
  SameName_ d n n rn = ()
  SameName_ d n rn_ rn = TypeError ("data and rep field mismatch for " <> 'ShowType d <> ": " <> n <> " / " <> rn)

type family SameName d (n :: Symbol) (rn :: Symbol) :: Constraint where
  SameName d n n = ()
  SameName d n rn = SameName_ d n (AppendSymbol "_" rn) rn

type family JoinComma (ns :: [FieldInfo]) :: ErrorMessage where
  JoinComma ('FieldInfo n : n1 : ns) = 'Text n <> ", " <> JoinComma (n1 : ns)
  JoinComma '[ 'FieldInfo n] = 'Text n

type family Align d ns fs rns reps :: Constraint where
  Align d ('FieldInfo n : ns) (f : fs) ('FieldInfo rn : rns) (Flatten rep : reps) =
    (SameName d n rn, Align d ns fs rns reps, AlignColumns rep f)
  Align d ('FieldInfo n : ns) (f : fs) ('FieldInfo rn : rns) (rep : reps) =
    (SameName d n rn, Align d ns fs rns reps)
  Align d '[] '[] '[] '[] = ()
  Align d '[] '[] rns reps = TypeError ('Text "too many fields in rep for " <> 'ShowType d <> ": " <> JoinComma rns)
  Align d ns fs '[] reps = TypeError ('Text "missing fields in rep for " <> 'ShowType d <> ": " <> JoinComma ns)

class AlignColumns rep d where

instance (
    IsRecord d ds name ns,
    IsRecord rep reps rname rns,
    Align d ns ds rns reps
  ) => AlignColumns rep d where

instance
  {-# OVERLAPPABLE #-}
  (
    IsRecord d ds name ns,
    IsRecord rep reps rname rns,
    AlignColumns rep d,
    GenColumns' reps ns ds
  ) => GenColumns rep d where
    genColumns =
      genColumns' @reps @ns @ds

instance
  (
    IsRecord d ds name ns,
    GenColumns' Auto' ns ds
  ) => GenColumns Auto d where
    genColumns =
      genColumns' @Auto' @ns @ds
