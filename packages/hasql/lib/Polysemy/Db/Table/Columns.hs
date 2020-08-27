{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Polysemy.Db.Table.Columns where

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
import Polysemy.Db.Table.ColumnType (ColumnType(columnType))
import Polysemy.Db.Table.Identifier (dbIdentifier)
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

class GenColumn t d r where
  genColumn :: Column

instance (
    DemoteFieldInfo n d,
    ColumnType d,
    ExplicitColumnParams r,
    ImplicitColumnParams d
  ) => GenColumn n d r where
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

class GenColumns' ns ds reps where
  genColumns' :: NonEmpty Column

instance {-# OVERLAPPABLE #-} (
    GenColumn n d r
  ) => GenColumns' '[n] '[d] '[r] where
    genColumns' =
      pure (genColumn @n @d @r)

instance GenColumns d rep => GenColumns' '[n] '[d] '[Flatten rep] where
    genColumns' =
      genColumns @d @rep

type family HeadRep rep where
  HeadRep Auto' = Auto
  HeadRep (r : r1 : reps) = r

type family TailRep rep where
  TailRep Auto' = Auto'
  TailRep (r : r1 : reps) = r1 : reps
  TailRep '[r] = TypeError ('Text "too few types in rep for GenColumns after '" <> 'ShowType r <> "'")

instance {-# OVERLAPPABLE #-} (
    GenColumn n d (HeadRep reps),
    GenColumns' (n1 : ns) (d1 : ds) (TailRep reps)
  ) => GenColumns' (n : n1 : ns) (d : d1 : ds) reps where
    genColumns' =
      genColumn @n @d @(HeadRep reps) :| toList (genColumns' @(n1 : ns) @(d1 : ds) @(TailRep reps))

instance (
    GenColumns d rep,
    GenColumns' (n1 : ns) (d1 : ds) (r1 : reps)
  ) => GenColumns' (n : n1 : ns) (d : d1 : ds) (Flatten rep : r1 : reps) where
    genColumns' =
      genColumns @d @rep <> genColumns' @(n1 : ns) @(d1 : ds) @(r1 : reps)

class GenColumns d rep where
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
    (SameName d n rn, Align d ns fs rns reps, AlignT f rep)
  Align d ('FieldInfo n : ns) (f : fs) ('FieldInfo rn : rns) (rep : reps) =
    (SameName d n rn, Align d ns fs rns reps)
  Align d '[] '[] '[] '[] = ()
  Align d '[] '[] rns reps = TypeError ('Text "too many fields in rep for " <> 'ShowType d <> ": " <> JoinComma rns)
  Align d ns fs '[] reps = TypeError ('Text "missing fields in rep for " <> 'ShowType d <> ": " <> JoinComma ns)

class AlignT d rep where

instance (
    IsRecord d ds name ns,
    IsRecord rep reps rname rns,
    Align d ns ds rns reps
  ) => AlignT d rep where

instance
  {-# OVERLAPPABLE #-}
  (
    IsRecord d ds name ns,
    IsRecord rep reps rname rns,
    AlignT d rep,
    GenColumns' ns ds reps
  ) => GenColumns d rep where
    genColumns =
      genColumns' @ns @ds @reps

instance
  (
    IsRecord d ds name ns,
    GenColumns' ns ds Auto'
  ) => GenColumns d Auto where
    genColumns =
      genColumns' @ns @ds @Auto'
