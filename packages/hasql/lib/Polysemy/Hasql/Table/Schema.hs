module Polysemy.Hasql.Table.Schema where

import Polysemy.Db.Data.Column (Auto, PrimQuery, UidRep, PrimaryKey)
import Polysemy.Db.Data.Uid (Uid)

import qualified Polysemy.Hasql.Data.QueryTable as Data
import Polysemy.Hasql.QueryParams (QueryParams(queryParams))
import Polysemy.Hasql.Table.BasicSchema (BasicSchema(basicSchema))
import Polysemy.Hasql.Tree.Table (TableRoot)
import Polysemy.Hasql.Where (Where(queryWhere))

-- |Derives a full 'QueryTable' using a represenation type.
-- Given a record type:
--
-- @
-- data User = User { id :: Int, name :: Text } deriving Generic
-- @
--
-- the representation type could look like this:
--
-- @
-- data UserRep = UserRep { id :: PrimaryKey, name :: Prim } deriving Generic
-- @
--
-- indicating that the @id@ column should be created with a @primary key@ option, and the @name@ column should be an
-- ordinary primitive.
class (
    BasicSchema rep d
  ) => Schema (qrep :: *) (rep :: *) (q :: *) (d :: *) where
    schema :: Data.QueryTable q d

instance (
    TableRoot rep d dTree,
    TableRoot qrep q qTree,
    BasicSchema rep d,
    QueryParams qTree q,
    Where qrep qTree q dTree d
  ) => Schema qrep rep q d where
    schema =
      Data.QueryTable (basicSchema @rep @d) (queryParams @qTree @q) (queryWhere @qrep @qTree @_ @dTree)

queryTable ::
  ∀ q d .
  Schema Auto Auto q d =>
  Data.QueryTable q d
queryTable =
  schema @Auto @Auto

type UidSchemaWith qrep rep i d =
  Schema qrep (UidRep PrimaryKey rep) i (Uid i d)

type UidSchema rep i d =
  UidSchemaWith (PrimQuery "id") rep i d
