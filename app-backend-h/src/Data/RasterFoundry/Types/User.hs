module Data.RasterFoundry.Types.User (
  User (..),
  Create (..),
  getUser
) where

import           Data.Aeson
import           Data.RasterFoundry.Types.Time (SqlTime, now) 
import           GHC.Generics                  (Generic)

newtype DropboxCredential = DropboxCredential String deriving (Eq, Show, Generic)
instance ToJSON DropboxCredential
instance FromJSON DropboxCredential

newtype PlanetCredential = PlanetCredential String deriving (Eq, Show, Generic)
instance ToJSON PlanetCredential
instance FromJSON PlanetCredential

data User = User { id                :: String
                 , createdAt         :: SqlTime
                 , modifiedAt        :: SqlTime
                 , dropboxCredential :: Maybe DropboxCredential
                 , planetCredential  :: Maybe PlanetCredential
                 , emailNotification :: Bool
                 , email             :: String
                 , name              :: String } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

data Create = Create { _id    :: String
                     , _email :: Maybe String } deriving (Eq, Show, Generic)

instance FromJSON Create where
  parseJSON = withObject "User" $ \v ->
    Create <$> v .: "id" <*> v .: "email"

instance ToJSON Create where
  toJSON (Create userId userEmail) =
    object ["id" .= userId, "email" .= userEmail]

getUser :: IO User
getUser = do
  currentTime <- now
  return User { Data.RasterFoundry.Types.User.id = "default"
              , createdAt = currentTime
              , modifiedAt = currentTime
              , dropboxCredential = Nothing
              , planetCredential = Nothing
              , emailNotification = False
              , email = "a-user@where-the-users-live.com"
              , name = "James"}
