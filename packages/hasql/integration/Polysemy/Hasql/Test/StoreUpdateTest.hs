module Polysemy.Hasql.Test.StoreUpdateTest where

-- import qualified Data.Aeson as Aeson
-- import qualified Data.List.NonEmpty as NonEmpty
-- import Exon (exon)
-- import Polysemy.Db.Data.DbError (DbError)
-- import Polysemy.Db.Data.Partial (Partial, partial)
-- import Polysemy.Db.Data.Rep (Auto, IdQueryAs, Prim, PrimaryKey, Rep, Sum)
-- import qualified Polysemy.Db.Effect.Store as Store
-- import Polysemy.Db.Effect.Store (Store)
-- import qualified Sqel.Data.Uid as Uid
-- import Sqel.Data.Uid (Uid (Uid))
-- import Polysemy.Db.Tree.Partial (field, (+>))
-- import Polysemy.Db.Tree.Partial.Insert (type (@>))
-- import Polysemy.Test (UnitTest, assertJust)

-- import Polysemy.Hasql.Test.Database (withTestStoreUidAs)
-- import Polysemy.Hasql.Test.Run (integrationTest)
-- import Polysemy.Db.Effect.Query (Query)
-- import qualified Polysemy.Db.Effect.Query as Query
-- import Polysemy.Hasql.Query.Query (interpretStoreQueryPartialUpdate)

-- newtype Tex =
--   Tex { unTex :: Text }
--   deriving stock (Eq, Show, Generic)
--   deriving newtype (IsString)

-- json ''Tex

-- newtype Buul =
--   Buul { unBuul :: Bool }
--   deriving stock (Eq, Show, Generic)

-- json ''Buul

-- data Dat =
--   Dat {
--     intField :: Int,
--     double :: Double,
--     txt :: Tex,
--     _boo :: Buul
--   }
--   deriving stock (Eq, Show, Generic)

-- data DatRep =
--   DatRep {
--     intField :: Prim,
--     double :: Prim,
--     txt :: Prim,
--     _boo :: Prim
--   }
--   deriving stock (Eq, Show, Generic)

-- data SumId =
--   S1 { sid1 :: Int }
--   |
--   S2 { sid2 :: Int }
--   deriving stock (Eq, Show, Generic, Ord)

-- data SumIdRep =
--   S1Rep { sid1 :: Prim }
--   |
--   S2Rep { sid2 :: Prim }
--   deriving stock (Eq, Show, Generic)

-- updateRecord :: Uid SumId Dat
-- updateRecord =
--   Uid (S1 1) (Dat 9 12.7 "text" (Buul True))

-- keepRecord :: Uid SumId Dat
-- keepRecord =
--   Uid (S1 2) (Dat 1 1 "one" (Buul True))

-- type DatUpdates =
--   ["intField" @> Int, "double" @> Double, "txt" @> Tex, "boo" @> Buul]

-- update ::
--   Partial Dat
-- update =
--   partial @Dat +> field @"intField" (5 :: Int) +> field @"double" (73.18 :: Double)

-- updateWith ::
--   ∀ e r .
--   Members [Query (SumId, Partial Dat) (Maybe Dat) !! e, Stop e] r =>
--   Partial Dat ->
--   Sem r ()
-- updateWith upd =
--   restop (void (Query.query (S1 1, upd)))

-- prog ::
--   ∀ e r .
--   Members [Query (SumId, Partial Dat) (Maybe Dat) !! e, Store SumId Dat !! e, Stop e, Error Text] r =>
--   Sem r (Maybe (NonEmpty (Uid SumId Dat)))
-- prog = do
--   restop (Store.insert updateRecord)
--   restop (Store.insert keepRecord)
--   updateWith update
--   restop (Query.query (S1 1, (partial @Dat +> field @"intField" (99 :: Int) +> field @"_boo" (Buul False))))
--   jsonUpdate <- fromEither (first toText (Aeson.eitherDecode [exon|{"txt":"updated"}|]))
--   updateWith jsonUpdate
--   restop Store.fetchAll

-- target :: NonEmpty (Uid SumId Dat)
-- target =
--   [Uid (S1 1) (Dat 99 73.18 "updated" (Buul False)), keepRecord]

-- test_partialDbUpdate :: UnitTest
-- test_partialDbUpdate =
--   integrationTest do
--     withTestStoreUidAs @(IdQueryAs (Sum SumIdRep)) @(Rep [PrimaryKey, Sum Auto]) $
--       interpretQueryUid @(IdQueryAs (Sum SumIdRep)) @(Rep [PrimaryKey, Sum Auto]) @Auto $
--       interpretStoreQueryPartialUpdate do
--         result <- prog @DbError
--         assertJust target (NonEmpty.sortWith Uid.id <$> result)
