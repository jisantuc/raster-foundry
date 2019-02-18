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

someAndFilters :: [Filter.AndFilter]
someAndFilters = [ Filter.andFilter "created_at < '2018-10-23'"
                 , Filter.andFilter "created_at > '2018-07-19'"
                 ]

someOrFilters :: [Filter.OrFilter]
someOrFilters = [ Filter.orFilter "owner = 'a bogus user who does not exist'"
                 , Filter.orFilter "created_at > '2018-07-19'"
                 ]

main :: IO ()
main = do
  print "you're doing great"
