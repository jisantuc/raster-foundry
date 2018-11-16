module Data.RasterFoundry.Types.Project (
  Project (..),
  Create (..),
  testProject,
  toProject
) where

import           Data.Aeson
import           Data.ByteString.Builder              (Builder, lazyByteString)
import           Data.ByteString.Lazy                 (fromStrict)
import           Data.Maybe                           (fromMaybe)
import           Data.RasterFoundry.Types.Time        (SqlTime, now)
import           Data.RasterFoundry.Types.Visibility  (Visibility(..))
import qualified Data.Text                            as T
import           Data.UUID                            (UUID, nil)
import qualified Data.UUID.V4                         as UUIDv4
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.FromRow   as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres
import qualified Database.PostgreSQL.Simple.ToRow     as Postgres
import qualified Database.PostgreSQL.Simple.Types     as Postgres
import           GHC.Generics                         (Generic)

data SingleBandOptions = SingleBandOptions { whichBand     :: Int } deriving (Eq, Show, Generic)
instance ToJSON SingleBandOptions
instance FromJSON SingleBandOptions
instance Postgres.ToField SingleBandOptions where
  toField = Postgres.Plain . Postgres.inQuotes . lazyByteString . encode
instance Postgres.FromField SingleBandOptions where
  fromField _ v = pure (fromMaybe (SingleBandOptions 0) $ fromStrict <$> v >>= decode)

data GeometryType = Polygon deriving (Eq, Show, Generic)
instance ToJSON GeometryType where
  toJSON Polygon = "Polygon"
instance FromJSON GeometryType where
  parseJSON (String v) = pure . toGeom $ v
    where
      toGeom :: T.Text -> GeometryType
      toGeom "Polygon" = Polygon
      toGeom s         = error "nah"

type Point = (Double, Double)

data Geometry = Polygon1 { coordinates :: [[Point]]
                         , _type       :: GeometryType } deriving (Eq, Show)
instance FromJSON Geometry where
  parseJSON =
    withObject "Polygon" $ \v -> Polygon1
    <$> v .: "coordinates"
    <*> v .: "type"
instance ToJSON Geometry where
  toJSON (Polygon1 coordinates geom) =
    object ["coordinates" .= coordinates, "type" .= show geom]
instance Postgres.ToField Geometry where
  toField = Postgres.Plain . Postgres.inQuotes . lazyByteString . encode
instance Postgres.FromField Geometry where
  fromField _ v = pure (fromMaybe (Polygon1 [] Polygon) $ fromStrict <$> v >>= decode)

data Project = Project { id                :: UUID
                       , createdAt         :: SqlTime
                       , modifiedAt        :: SqlTime
                       , createdBy         :: String
                       , modifiedBy        :: String
                       , name              :: String
                       , slugLabel         :: String
                       , description       :: String
                       , visibility        :: Visibility
                       , tags              :: Postgres.PGArray String
                       , manualOrder       :: Bool
                       , extent            :: Maybe Geometry
                       , tileVisibility    :: Visibility
                       , isAOIProject      :: Bool
                       , aoiCadenceMillis  :: Integer
                       , aoisLastChecked   :: SqlTime
                       , owner             :: String
                       , isSingleBand      :: Bool
                       , singleBandOptions :: Maybe SingleBandOptions
                       } deriving (Eq, Show, Generic)

-- instance ToJSON Project
-- instance FromJSON Project

instance Postgres.ToRow Project
instance Postgres.FromRow Project

data Create = Create { _name              :: String
                     , _description       :: String
                     , _isAOIProject      :: Bool
                     , _aoiCadenceMillis  :: Integer
                     , _owner             :: Maybe String
                     , _tags              :: Postgres.PGArray String
                     , _isSingleBand      :: Bool
                     , _singleBandOptions :: Maybe SingleBandOptions
                     , _extras            :: Value } deriving (Eq, Show, Generic)

toProject :: Create -> String -> IO Project
toProject create userId = do
  projectId <- UUIDv4.nextRandom
  currentTime <- now
  return Project { Data.RasterFoundry.Types.Project.id = projectId
                 , createdAt = currentTime
                 , modifiedAt = currentTime
                 , createdBy = userId
                 , modifiedBy = userId
                 , owner = fromMaybe "default" $ _owner create
                 , name = _name create
                 , description = _description create
                 , isAOIProject = False
                 , aoiCadenceMillis = 86400
                 , aoisLastChecked = currentTime
                 , tags = _tags create
                 , isSingleBand = _isSingleBand create
                 , singleBandOptions = _singleBandOptions create
                 , extent = Nothing
                 , slugLabel = "lol"
                 , visibility = Private
                 , tileVisibility = Private
                 , manualOrder = False }

testProject :: Create
testProject = Create { _name = "a quite good project"
                     , _description = "very good very good indeed"
                     , _isAOIProject = False -- it could hardly be quite good if it were an AOI
                     , _aoiCadenceMillis = 0
                     , _owner = Nothing
                     , _tags = Postgres.PGArray ["a", "quite", "good", "project"]
                     , _isSingleBand = False
                     , _singleBandOptions = Nothing
                     , _extras = Null }
