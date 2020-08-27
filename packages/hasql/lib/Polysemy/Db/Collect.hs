module Polysemy.Db.Collect where

import Polysemy.Db.Data.Column (Auto, Flatten, Prim)

data TvShowRep =
  TvShowRep {
    showId :: Prim Auto,
    name :: Prim Auto
  }
  deriving (Eq, Show)

deriveGeneric ''TvShowRep

data EpisodeRep =
  EpisodeRep {
    showName :: Prim Auto,
    season :: Prim Auto,
    episode :: Prim Auto,
    showId :: Prim Auto,
    episodeId :: Prim Auto
  }
  deriving (Eq, Show)

deriveGeneric ''EpisodeRep

data SeasonRep =
  SeasonRep {
    showName :: Prim Auto,
    season :: Prim Auto,
    showId :: Prim Auto
  }
  deriving (Eq, Show)

deriveGeneric ''SeasonRep

data TorrentMetaRep =
  TorrentMetaRep {
    magnet :: Prim Auto,
    resolution :: Prim Auto,
    audio :: Prim Auto,
    codec :: Prim Auto
  }
  deriving (Eq, Show)

deriveGeneric ''TorrentMetaRep

data ShowOptionsRep =
  ShowOptionsRep {
    searchName :: Prim Auto,
    searchType :: Prim Auto,
    fetchLatest :: Prim Auto
  }
  deriving (Eq, Show)

deriveGeneric ''ShowOptionsRep

data FeedShowRep =
  FeedShowRep {
    show :: Flatten TvShowRep,
    options :: Flatten ShowOptionsRep
  }
  deriving (Eq, Show)

deriveGeneric ''FeedShowRep

data TrackShowRep =
  TrackShowRep {
    tvShow :: Flatten TvShowRep
  }
  deriving (Eq, Show)

deriveGeneric ''TrackShowRep

data PlanShowRep =
  PlanShowRep {
    tvShow :: Flatten TvShowRep
  }
  deriving (Eq, Show)

deriveGeneric ''PlanShowRep

data ScheduleEpisodeRep =
  ScheduleEpisodeRep {
    episode :: Flatten EpisodeRep,
    airdate :: Prim Auto
  }
  deriving (Eq, Show)

deriveGeneric ''ScheduleEpisodeRep

data SearchEpisodeRep =
  SearchEpisodeRep {
    episode :: Flatten EpisodeRep
  }
  deriving (Eq, Show)

deriveGeneric ''SearchEpisodeRep

data SearchSeasonRep =
  SearchSeasonRep {
    season :: Flatten SeasonRep
  }
  deriving (Eq, Show)

deriveGeneric ''SearchSeasonRep

data AcquireEpisodeRep =
  AcquireEpisodeRep {
    episode :: Flatten EpisodeRep,
    torrent :: Flatten TorrentMetaRep
  }
  deriving (Eq, Show)

deriveGeneric ''AcquireEpisodeRep

data AcquireSeasonRep =
  AcquireSeasonRep {
    season :: Flatten SeasonRep,
    torrent :: Flatten TorrentMetaRep
  }
  deriving (Eq, Show)

deriveGeneric ''AcquireSeasonRep

data FetchEpisodeRep =
  FetchEpisodeRep {
    episode :: Flatten EpisodeRep,
    fileId :: Prim Auto
  }
  deriving (Eq, Show)

deriveGeneric ''FetchEpisodeRep

data FetchAnyRep =
  FetchAnyRep {
    title :: Prim Auto,
    fileId :: Prim Auto
  }
  deriving (Eq, Show)

deriveGeneric ''FetchAnyRep

data ArchiveRep =
  ArchiveRep {
    episode :: Flatten EpisodeRep,
    path :: Prim Auto
  }
  deriving (Eq, Show)

deriveGeneric ''ArchiveRep

data CompletedEpisodeRep =
  CompletedEpisodeRep {
    episode :: Flatten EpisodeRep,
    path :: Prim Auto
  }
  deriving (Eq, Show)

deriveGeneric ''CompletedEpisodeRep

data CompletedAnyRep =
  CompletedAnyRep {
    name :: Prim Auto
  }
  deriving (Eq, Show)

deriveGeneric ''CompletedAnyRep

data SearchResultLogRep =
  SearchResultLogRep {
    results :: Prim Auto
  }
  deriving (Eq, Show)

deriveGeneric ''SearchResultLogRep
