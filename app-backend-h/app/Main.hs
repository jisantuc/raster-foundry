module Main where

import           Data.Aeson                       (decode, encode)
import           Data.ByteString                  (ByteString)
import qualified Data.RasterFoundry.Types.Project as Project
import           Data.RasterFoundry.Types.User    (User, getUser)
import qualified Database.PostgreSQL.Simple       as Postgres
import qualified Database.RasterFoundry           as Database
import           Database.RasterFoundry.Project

connString :: ByteString
connString = "postgresql://rasterfoundry:rasterfoundry@localhost:5432/rasterfoundry"

main :: IO ()
main = do
  conn <- Postgres.connectPostgreSQL connString
  user <- getUser
  handle <- pure $ Database.Handle conn
  result <- createProject handle Project.testProject user
  fetchResult <- getProject handle (Project.id . head $ result)
  print result
  print fetchResult
