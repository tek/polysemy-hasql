module Polysemy.Hasql.Table.Query.Where where

import qualified Data.Text as Text
import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (ConstructorInfo(Record), DatatypeInfo(Newtype, ADT), FieldInfo(FieldInfo))

import Fcf (Eval, Zip)
import Polysemy.Db.Data.Cond (Greater, GreaterOrEq, Less, LessOrEq)
import Polysemy.Db.SOP.Constraint (dataSlugSymbol_)
import qualified Polysemy.Hasql.Data.QueryWhere as Data (QueryWhere(QueryWhere))
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Hasql.Table.Query.Prepared (dollar)

fieldWithOp ::
  ∀ (name :: Symbol) .
  KnownSymbol name =>
  Text ->
  Int ->
  Text
fieldWithOp op index =
  [qt|"#{dataSlugSymbol_ @name}" #{op} #{dollar index}|]

regularField ::
  ∀ (name :: Symbol) .
  KnownSymbol name =>
  Int ->
  Text
regularField =
  fieldWithOp @name "="

maybeField ::
  Int ->
  Text ->
  Text
maybeField index cond =
  [qt|(#{dollar index} is null or #{cond})|]

concatWhereFields ::
  [Int -> Text] ->
  Text
concatWhereFields fields =
  Text.intercalate " and " (zipWith ($) fields [(1 :: Int)..length fields])

where' ::
  [Int -> Text] ->
  Data.QueryWhere a query
where' fields =
  Data.QueryWhere (SqlCode (concatWhereFields fields))

class QueryWhereField d q name where
  queryWhereField :: Int -> Text

instance {-# overlappable #-} KnownSymbol name => QueryWhereField d d name where
  queryWhereField =
    regularField @name

instance {-# overlappable #-} QueryWhereField d q name => QueryWhereField d (Maybe q) name where
  queryWhereField i =
    maybeField i (queryWhereField @d @q @name i)

instance {-# overlappable #-} QueryWhereField d q name => QueryWhereField (Maybe d) q name where
  queryWhereField =
    queryWhereField @d @q @name

instance {-# overlappable #-} KnownSymbol name => QueryWhereField d (Less q) name where
  queryWhereField =
    fieldWithOp @name "<"

instance {-# overlappable #-} KnownSymbol name => QueryWhereField d (LessOrEq q) name where
  queryWhereField =
    fieldWithOp @name "<="

instance {-# overlappable #-} KnownSymbol name => QueryWhereField d (Greater q) name where
  queryWhereField =
    fieldWithOp @name ">"

instance {-# overlappable #-} KnownSymbol name => QueryWhereField d (GreaterOrEq q) name where
  queryWhereField =
    fieldWithOp @name ">="

class QueryWhereFields (ass :: [(*, FieldInfo)]) (qss :: [(*, FieldInfo)]) where
  queryWhereFields :: [Int -> Text]

instance QueryWhereFields '[] '[] where
  queryWhereFields =
    mempty

instance {-# overlappable #-} QueryWhereFields ass qss => QueryWhereFields (a : ass) qss where
  queryWhereFields =
    queryWhereFields @ass @qss

instance (
    QueryWhereField d q name,
    QueryWhereFields ass qss
  ) => QueryWhereFields ('(d, 'FieldInfo name) : ass) ('(q, 'FieldInfo name) : qss) where
  queryWhereFields =
    queryWhereField @d @q @name : queryWhereFields @ass @qss

type family Fields' (ass :: [[*]]) (info :: DatatypeInfo) :: [(*, FieldInfo)] where
  Fields' '[as] ('Newtype _ _ ('Record _ fs)) = Eval (Zip as fs)
  Fields' '[as] ('ADT _ _ '[('Record _ fs)] _) = Eval (Zip as fs)

type Fields a =
  Fields' (GCode a) (GDatatypeInfoOf a)

-- Construct a @where@ fragment from two types, validating that all fields of the query record and their types are
-- present and matching in the data record
class QueryWhere a query where
  queryWhere :: Data.QueryWhere a query

instance (
    QueryWhereFields (Fields d) (Fields query)
  ) =>
  QueryWhere d query where
    queryWhere =
      where' (queryWhereFields @(Fields d) @(Fields query))
