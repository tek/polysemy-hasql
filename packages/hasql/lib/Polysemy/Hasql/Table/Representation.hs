module Polysemy.Hasql.Table.Representation where

class Representation (ass :: [[*]]) (repss :: [[*]]) | ass -> repss where
