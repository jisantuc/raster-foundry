module Data.RasterFoundry.Types.Visibility (
  Visibility(..)
  ) where

import qualified Data.ByteString.Char8                as B
import           Data.ByteString.Lazy                 (ByteString)
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres

data Visibility = Private | Public deriving (Eq, Show)

fromString :: String -> Visibility
fromString "PUBLIC"  = Public
fromString "PRIVATE" = Private
fromString s         = error $ "Not a valid visibility type: " ++ show s

instance Postgres.FromField Visibility where
  fromField f v =
    case B.unpack <$> v of
      Nothing -> Postgres.returnError Postgres.UnexpectedNull f ""
      Just dat ->
        pure $ fromString dat

instance Postgres.ToField Visibility where
  toField Public  = Postgres.Escape $ "PUBLIC"
  toField Private = Postgres.Escape $ "PRIVATE"
