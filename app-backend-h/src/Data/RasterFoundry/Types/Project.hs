module Data.RasterFoundry.Types.Project (
  Project (..),
  Create (..),
  getProject
) where

import           Data.Aeson
import           Data.RasterFoundry.Types.Time (SqlTime, now)
import qualified Data.Text                     as T
import           Data.UUID                     (UUID, nil)
import qualified Data.UUID.V4                  as UUIDv4
import           GHC.Generics                  (Generic)

data SingleBandOptions = SingleBandOptions { whichBand     :: Int } deriving (Eq, Show, Generic)
instance ToJSON SingleBandOptions
instance FromJSON SingleBandOptions

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

data Project = Project { id               :: UUID
                       , createdAt        :: SqlTime
                       , modifiedAt       :: SqlTime
                       , createdBy        :: String
                       , modifiedBy       :: String
                       , owner            :: String
                       , name             :: String
                       , description      :: String
                       , isAOIProject     :: Bool
                       , aoiCadenceMillis :: Integer
                       , aoisLastChecked  :: SqlTime
                       , tags             :: [String]
                       , extent           :: Maybe Geometry } deriving (Eq, Show, Generic)

instance ToJSON Project
instance FromJSON Project

data Create = Create { _name              :: String
                     , _description       :: String
                     , _isAOIProject      :: Bool
                     , _aoiCadenceMillis  :: Integer
                     , _owner             :: Maybe String
                     , _tags              :: [String]
                     , _isSingleBand      :: Bool
                     , _singleBandOptions :: Maybe SingleBandOptions
                     , _extras            :: Value } deriving (Eq, Show, Generic)

getProject :: IO Project
getProject = do
  projectId <- UUIDv4.nextRandom
  currentTime <- now
  return Project { Data.RasterFoundry.Types.Project.id = projectId
                 , createdAt = currentTime
                 , modifiedAt = currentTime
                 , createdBy = "a user"
                 , modifiedBy = "a user"
                 , owner = "a user"
                 , name = "a quite good project"
                 , description = "very good very good indeed"
                 , isAOIProject = False -- it could hardly be quite good if it were an AOI
                 , aoiCadenceMillis = 0
                 , aoisLastChecked = currentTime
                 , tags = ["a", "quite", "good", "project"]
                 , extent = Just $ Polygon1 [ [ (0, 0), (1, 0), (1, 1), (0, 1), (0, 0)]] Polygon}
