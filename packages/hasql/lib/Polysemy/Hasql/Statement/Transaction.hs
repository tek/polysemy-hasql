module Polysemy.Hasql.Statement.Transaction where

import Exon (exon)
import Hasql.Decoders (noResult)
import Hasql.Encoders (noParams)
import Hasql.Statement (Statement (Statement))

data AccessMode =
  Read
  |
  Write
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data IsolationLevel =
  ReadCommitted
  |
  RepeatableRead
  |
  Serializable
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data TransactionConfig =
  TransactionConfig {
    isolationLevel :: IsolationLevel,
    accessMode :: AccessMode,
    deferrable :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance Default TransactionConfig where
  def =
    TransactionConfig {
      isolationLevel = ReadCommitted,
      accessMode = Write,
      deferrable = False
    }

beginTransactionSql :: TransactionConfig -> ByteString
beginTransactionSql TransactionConfig {..} =
  [exon|start transaction #{isolation isolationLevel} #{mode accessMode} #{defer}|]
  where
    isolation = \case
      ReadCommitted -> "isolation level read committed"
      RepeatableRead -> "isolation level repeatable read"
      Serializable -> "isolation level serializable"
    mode = \case
      Write -> "read write"
      Read -> "read only"
    defer | deferrable = "deferrable"
          | otherwise = ""

beginTransaction :: TransactionConfig -> Bool -> Statement () ()
beginTransaction conf preparable =
  Statement (beginTransactionSql conf) noParams noResult preparable

commitTransaction :: Bool -> Statement () ()
commitTransaction preparable =
  Statement "commit" noParams noResult preparable

rollbackTransaction :: Bool -> Statement () ()
rollbackTransaction preparable =
  Statement "rollback" noParams noResult preparable
