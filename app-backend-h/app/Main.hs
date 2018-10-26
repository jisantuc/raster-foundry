module Main where

import           Data.Aeson                       (decode, encode)
import           Data.RasterFoundry.Types.Project (Project, getProject)
import           Data.RasterFoundry.Types.User    (User, getUser)

main :: IO ()
main = do
  project <- getProject
  user <- getUser
  print "Project enc-/decoding works:"
  print $ (encode <$> Just project) ==
           (encode <$> ((decode . encode $ project) :: Maybe Project))
  print "User enc-/decoding works:"
  print $ (encode <$> Just user)
           == (encode <$> ((decode . encode $ user) :: Maybe User))
