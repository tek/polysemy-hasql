module Polysemy.Hasql.Table.Query.Insert where

import qualified Polysemy.Db.Data.TableStructure as Column (Column(..))
import qualified Polysemy.Db.Data.TableStructure as TableStructure (TableStructure(..))
import Polysemy.Db.Data.TableStructure (Column, CompositeType(CompositeType), TableStructure(TableStructure))
import Polysemy.Hasql.Data.SqlCode (SqlCode(..))
import Polysemy.Hasql.Table.Query.Fragment (intoFragment)
import Polysemy.Hasql.Table.Query.Text (commaColumns, commaSeparated)

row :: Text -> Text
row a =
  [qt|row(#{a})|]

dollar :: Int -> Text
dollar i =
  [qt|$#{i}|]

compositeConstructor :: Int -> NonEmpty Column -> (Int, Text)
compositeConstructor index columns =
  (newIndex, row cols)
  where
    cols =
      commaSeparated @[] (dollar <$> [index..newIndex - 1])
    newIndex =
      index + length columns

compositeColumns :: Int -> NonEmpty TableStructure -> (Int, Text)
compositeColumns index structs =
  (newIndex, row (commaSeparated (sumIndex <| conss)))
  where
    sumIndex =
      dollar index
    (newIndex, conss) =
      mapAccumL compositeConstructor (index + 1) (TableStructure._columns <$> structs)

compositeColumnCount :: NonEmpty TableStructure -> Int
compositeColumnCount =
  sum . fmap (length . TableStructure._columns)

insertValues :: [Column] -> [Text]
insertValues =
  snd . mapAccumL column 1 . fmap Column.customType
  where
    column index = \case
      Just (CompositeType _ _ composites) ->
        compositeColumns index composites
      Nothing ->
        (index + 1, dollar index)

insert ::
  TableStructure ->
  SqlCode
insert (TableStructure (intoFragment -> SqlCode into) columns) =
  SqlCode [qt|insert #{into} (#{cols}) values (#{values})|]
  where
    cols =
      commaColumns columns
    values =
      commaSeparated (insertValues (toList columns))
