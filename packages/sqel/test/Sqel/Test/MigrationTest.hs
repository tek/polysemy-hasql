{-# options_ghc -Wno-partial-type-signatures #-}

module Sqel.Test.MigrationTest where

import Exon (exon)
import Generics.SOP (Top, hcfoldMap)
import Hedgehog (TestT, (===))
import Prelude hiding (sum)

import Sqel.Data.Dd (Dd, DdK (DdK), type (:>) ((:>)))
import Sqel.Data.Migration (Migration (Migration), Migrations (Migrations), migrate, migration, tableFrom)
import Sqel.Data.Sql (Sql)
import Sqel.Merge (merge)
import Sqel.Migration.Consistency (tableStatements)
import Sqel.Migration.Statement (MigrationStatement, migrationStatementSql)
import Sqel.Migration.Table (migrateAuto)
import Sqel.Names (typeAs)
import Sqel.Prim (migrateRename, prim, primIndex, primNullable, prims)
import Sqel.Product (prod)
import Sqel.Sum (con, indexPrefix, sum, sumWith)

data Thing =
  Thing1 { x :: Int, y :: Int }
  |
  Thing2 { z :: Int, a :: Int }
  deriving stock (Eq, Show, Generic)

data Dat0 =
  Dat0 {
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

data Dat1 =
  Dat1 {
    num :: Maybe Int,
    name :: Text,
    thing :: Thing
  }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    num :: Maybe Int,
    name :: Text,
    thing :: Thing
  }
  deriving stock (Eq, Show, Generic)

dd_Dat0 :: Dd ('DdK _ _ Dat0 _)
dd_Dat0 =
  typeAs @"Dat" (prod prims)

dd_Dat1 :: Dd ('DdK _ _ Dat1 _)
dd_Dat1 =
  typeAs @"Dat" (prod (primNullable :> prim :> indexPrefix @"ph_sum_index__" (merge (sum (con prims :> con prims)))))

-- TODO add combi @migrateRenameIndex@
dd_Dat :: Dd ('DdK _ _ Dat _)
dd_Dat =
  prod (primNullable :> prim :> merge (sumWith (migrateRename @"ph_sum_index__Thing" (primIndex @"Thing")) (con prims :> con prims)))

migrations ::
  Migrations (Const [MigrationStatement]) '[Dat1, Dat0] Dat
migrations =
  migrate (
    migrateAuto dd_Dat1 dd_Dat :>
    migrateAuto dd_Dat0 dd_Dat1
  )

stmts :: [MigrationStatement]
stmts =
  let Migrations migs = migrations
  in hcfoldMap (Proxy @Top) (\ Migration {migration} -> getConst migration) migs

tableStmts :: [Sql]
tableStmts =
  let Migrations migs = migrations
  in hcfoldMap (Proxy @Top) (\ Migration {tableFrom} -> tableStatements tableFrom) migs

stmtsTarget :: [Sql]
stmtsTarget =
  [
    "alter table dat rename column ph_sum_index__thing to sqel_sum_index__thing",
    "alter table dat add column ph_sum_index__thing bigint",
    "alter table dat alter column ph_sum_index__thing set not null",
    "alter table dat add column thing1 sqel_type__thing1",
    "alter table dat alter column thing1 set not null",
    "alter table dat add column thing2 sqel_type__thing2",
    "alter table dat alter column thing2 set not null",
    "alter table dat add column num bigint"
  ]

tableStmtsTarget :: [Sql]
tableStmtsTarget =
  [
    [exon|create table "dat" ("num" bigint, "name" text not null, "ph_sum_index__thing" bigint not null, "thing1" sqel_type__thing1 not null, "thing2" sqel_type__thing2 not null)|],
    [exon|create type "sqel_type__thing1" as ("x" bigint, "y" bigint)|],
    [exon|create type "sqel_type__thing2" as ("z" bigint, "a" bigint)|],
    [exon|create table "dat" ("name" text not null)|]
  ]

test_migration :: TestT IO ()
test_migration = do
  stmtsTarget === (migrationStatementSql <$> stmts)
  tableStmtsTarget === tableStmts
