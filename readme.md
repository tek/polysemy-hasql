# Under Construction

# About

This Haskell library provides [Polysemy] effects for [Hasql], a bit of
abstraction of databases and some generic derivation utilities for database
schemas.

At this time, two use cases for schemas are addressed:

* CRUD operations on data type-mapped tables
* simple data type-mapped queries on data type-mapped tables

# Basic Example

```haskell
import Polysemy (runM, resourceToIO)
import Polysemy.Db
import Polysemy.Hasql

data Dat = Dat { number :: Int } deriving (Generic)

prog :: Member (Store Int e Dat) r => Sem r (Either e (Maybe Dat))
prog = do
  Store.upsert (Dat 5)
  Store.fetch 5

table :: QueryTable Dat Int
table =
  QueryTable (Table (TableStructure "dats" columns row params)) (QueryWhere statement)
  where
    row =
      Dat <$> Decoder.int8
    params =
      number >$< Encoder.int8
    columns =
      Columns (pure (Column "number" "bigint" def { primaryKey = True }))
    statement =
      Sql "number = $1"

main :: IO ()
main = do
  result <- runM $ interpretDbConnection $ interpretStoreDbFull table prog
  print result
```

As is apparent in `prog`, the business logic only uses a very simple
abstraction of database actions, encoded in the Polysemy effect `Store`.
The integration with Postgres is done by the interpreters in `main`, which use
[Hasql] to connect to a database and translate the queries into SQL.
Additionally, a potentially existing schema is examined upon connection and
compared to the declaration in the `TableStructure`, adding missing columns and
terminating with an error if there are any unsolvable incompatibilities.

Obviously, writing and maintaining these `QueryTable` definitions is messy and
error-prone, so the library offers a generic derivation mechanism to automate
it.

# Schemas

Deriving a table definition from the simple data type only would make it
impossible to configure columns.
Configuring them implicitly, by writing instances for some column metadata
class, might be a solution, but the idea for this library is to use a second
data type for the table configuration that is then typechecked against the
record type.

Given a data type:

```haskell
data Nested =
  Nested {
    elems :: [Text],
    num :: Double
  }
  deriving (Generic)

data Record =
  Record {
    id :: Int,
    nested :: Nested
  }
  deriving (Generic)
```

we define a database representation type:

```haskell
data NestedRep {
  elems :: Prim Auto,
  num :: Prim Auto
}
  deriving (Generic)

data RecordRep =
  RecordRep {
    id :: Prim PrimaryKey,
    nested :: Flatten
  }
  deriving (Generic)
```

and then pass it explicitly to a generic constructor:

```haskell
table = basicSchema @RecordRep
queryTable = schema @RecordRep @Int
-- TODO
schema @Auto
```

or use one of the interpreters:

```haskell
main :: IO ()
main = do
  result <- runM $ runError $ interpretStoreDbSingle @RecordRep prog
  print result
```

This will derive the Postgres column type, like `bigint`, and the [Hasql] `Row`
and `Params` codecs, based on the type used for the fields in the
representation type.
The field names are checked for congruence and smart type errors will be
emitted if they don't match:

```haskell
-- TODO
```

The types `Prim`, `Auto` etc. are just simple singleton markers that are used
to match in instances of `GenColumns`.
You can easily add your own markers to implement custom column metadata:

```haskell
-- TODO
```

# Queries

Non-CRUD queries are handled by the effect `Query`, defined by another
data type whose fields are a subset of the table type's:

```haskell
data RecordQuery =
  RecordQuery {
    elems :: [Text],
    number :: Double
  }
deriveGeneric ''RecordQuery

prog :: Member (Query RecordQuery (Maybe Record)) r => Sem r (Maybe Record)
prog =
  Query.query (RecordQuery ["one", "two"] 5.5)

main :: IO ()
main =
  print =<< (runM $ interpretDatabase $ interpretOneGen prog)
```

[Polysemy]: https://hackage.haskell.org/package/polysemy
[Hasql]: https://hackage.haskell.org/package/hasql
