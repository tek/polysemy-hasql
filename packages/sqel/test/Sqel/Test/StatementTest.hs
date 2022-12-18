module Sqel.Test.StatementTest where

import Hedgehog (TestT, (===))

import Sqel.Data.Order (Order (Desc))
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.Sql (Sql, sql)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim)
import Sqel.Product (prod)
import Sqel.Query (checkQuery)
import Sqel.Query.Combinators (order)
import qualified Sqel.Sql.Select as Sql

data Dat =
  Dat {
    num :: Int
  }
  deriving stock (Eq, Show, Generic)

data Q =
  Q {
    num :: ()
  }
  deriving stock (Eq, Show, Generic)

target :: Sql
target =
  [sql|select "num" from "dat" order by "num" desc|]

qs :: QuerySchema Q Dat
qs =
  checkQuery (prod (order Desc)) (prod prim)

ts :: TableSchema Dat
ts =
  tableSchema (prod prim)

test_statement_order :: TestT IO ()
test_statement_order =
  target === Sql.selectWhere qs ts
