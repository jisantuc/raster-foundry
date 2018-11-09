module Data.RasterFoundry.Types.Time (
  SqlTime (..),
  now
) where

import           Data.Aeson
import           Data.Either                          (fromRight)
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (encodeUtf8)
import           Data.Time.Clock                      (getCurrentTime)
import           Data.Time.Format                     (defaultTimeLocale,
                                                       formatTime)
import           Data.Time.LocalTime                  (utc, utcToLocalTime)
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import           Database.PostgreSQL.Simple.Time      (LocalTimestamp,
                                                       Unbounded (..),
                                                       localTimestampToBuilder,
                                                       parseLocalTimestamp)
import qualified Database.PostgreSQL.Simple.ToField   as Postgres

newtype SqlTime = SqlTime LocalTimestamp deriving (Eq, Show)
instance ToJSON SqlTime where
  toJSON (SqlTime (Finite t))  = toJSON . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" $ t
  toJSON (SqlTime NegInfinity) = toJSON . T.pack $ "1900-01-01T00:00:00"
  toJSON (SqlTime PosInfinity) = toJSON . T.pack $ "3000-01-01T00:00:00"
instance FromJSON SqlTime where
  parseJSON (String v) =
    pure . fromRight (SqlTime NegInfinity) $ SqlTime <$> boundTime v
    where
      boundTime :: T.Text -> Either String LocalTimestamp
      boundTime "1900-01-01T00:00:00Z" = Right NegInfinity
      boundTime "3000-01-01T00:00:00Z" = Right PosInfinity
      boundTime time                   = parseLocalTimestamp . encodeUtf8 $ v

instance Postgres.ToField SqlTime where
  toField (SqlTime t) = Postgres.Plain . Postgres.inQuotes . localTimestampToBuilder $ t
instance Postgres.FromField SqlTime where
  fromField f v = SqlTime <$> Postgres.fromField f v

now :: IO SqlTime
now = SqlTime . Finite <$> (utcToLocalTime utc <$> getCurrentTime)
