module Main where

import           Data.Aeson                        (decode, encode)
import           Data.ByteString                   (ByteString)
import qualified Data.RasterFoundry.Classes.Filter as Filter
import qualified Data.RasterFoundry.Types.Project  as Project
import           Data.RasterFoundry.Types.User     (User, getUser)
import qualified Database.PostgreSQL.Simple        as Postgres
import qualified Database.RasterFoundry            as Database
import           Database.RasterFoundry.Project

connString :: ByteString
connString = "postgresql://rasterfoundry:rasterfoundry@localhost:5432/rasterfoundry"

main :: IO ()
main = do
  print "you're doing great"
