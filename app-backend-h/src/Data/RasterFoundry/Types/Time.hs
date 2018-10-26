module Data.RasterFoundry.Types.Time (
  SqlTime (..),
  now
) where

import           Data.Aeson
import           Data.Either                     (fromRight)
import qualified Data.Text                       as T
import           Data.Text.Encoding              (encodeUtf8)
import           Data.Time.Clock                 (getCurrentTime)
import           Data.Time.Format                (defaultTimeLocale, formatTime)
import           Database.PostgreSQL.Simple.Time (UTCTimestamp, Unbounded (..),
                                                  parseUTCTimestamp)

newtype SqlTime = SqlTime UTCTimestamp deriving (Eq, Show)
instance ToJSON SqlTime where
  toJSON (SqlTime (Finite t))  = toJSON . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" $ t
  toJSON (SqlTime NegInfinity) = toJSON . T.pack $ "1900-01-01T00:00:00Z"
  toJSON (SqlTime PosInfinity) = toJSON . T.pack $ "3000-01-01T00:00:00Z"
instance FromJSON SqlTime where
  parseJSON (String v) =
    pure . fromRight (SqlTime NegInfinity) $ SqlTime <$> boundTime v
    where
      boundTime :: T.Text -> Either String UTCTimestamp
      boundTime "1900-01-01T00:00:00Z" = Right NegInfinity
      boundTime "3000-01-01T00:00:00Z" = Right PosInfinity
      boundTime time                   = parseUTCTimestamp . encodeUtf8 $ v

now :: IO SqlTime
now = SqlTime . Finite <$> getCurrentTime
