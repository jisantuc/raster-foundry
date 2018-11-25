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
  conn <- Postgres.connectPostgreSQL connString
  user <- getUser
  handle <- pure $ Database.Handle conn
  result <- createProject handle Project.testProject user
  fetchResult <- getProject handle (Project.id . head $ result)
  recentlyCheckedAOIs <- checkAOIs . head $ fetchResult
  updateResult <- updateProject handle recentlyCheckedAOIs user
  fetchAfterUpdate <- getProject handle (Project.id recentlyCheckedAOIs)
  print "And filters:"
  print $ Filter.toQuery . Filter.toFilter . mconcat $ someAndFilters
  andListResults <- listProjects handle (mconcat someAndFilters)
  print "Or filters:"
  print $ Filter.toQuery . Filter.toFilter . mconcat $ someOrFilters
  orListResults <- listProjects handle (mconcat someOrFilters)
  print "Result of project creation:"
  print result
  print "Result of fetching project by id after creation"
  print fetchResult
  print "Result of listing after and-ing two filters:"
  print andListResults
  print "Result of listing after or-ing two filters:"
  print orListResults
  print "Result of fetch after updating aois last checked:"
  print fetchAfterUpdate
