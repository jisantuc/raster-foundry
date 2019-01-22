import qualified Database.RasterFoundry as Database
import qualified Database.RasterFoundry.Project as Project
import           Data.ByteString                   (ByteString)
import qualified Data.RasterFoundry.Types.Project as Project

import Types
import Test.QuickCheck.Monadic

main :: IO ()
main = monadicIO

connString :: ByteString
connString = "postgresql://rasterfoundry:rasterfoundry@localhost:5432/rasterfoundry"

-- TODO I'm not really sure how this works.
-- Check out this gist and see if it will maybe help
-- https://gist.github.com/ijt/967505/d4296652c91d883f70145e85c3092ee4a50edcdf
roundTripProp :: IO (TestHandle -> TestProjectCreate -> Bool)
roundTripProp (TestHandle handleFactory) (TestProjectCreate create) =
  do
    handle <- handleFactory
    user <- undefined
    [inserted] <- Project.createProject handle create user
    pure $ Project.description inserted == Project._description create
