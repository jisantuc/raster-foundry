import qualified Database.PostgreSQL.Simple.Types as Postgres
import qualified Database.RasterFoundry as Database
import qualified Database.RasterFoundry.Project as Project
import           Data.ByteString                   (ByteString, pack)
import           Data.RasterFoundry.Classes.Filter
import qualified Data.RasterFoundry.Types.Project as Project
import qualified Data.RasterFoundry.Types.User as User

import Types
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

main :: IO ()
main = hspec $ do
  describe "some db tests" $ do
    it "should round trip a project through the db" $ do
      property insertRoundTripProp
    it "should get a project successfully" $ do
      property getProjectProp

connString :: ByteString
connString = "postgresql://rasterfoundry:rasterfoundry@localhost:5432/rasterfoundry"

insertRoundTripProp :: TestHandle -> TestProjectCreate -> Property
insertRoundTripProp (TestHandle handleFactory) (TestProjectCreate projCreate) = monadicIO $ do
  handle <- run handleFactory
  user <- run User.getUser
  [inserted] <- run $ Project.createProject handle projCreate user
  assert $ Project.description inserted == Project._description projCreate

getProjectProp :: TestHandle -> TestProjectCreate -> Property
getProjectProp (TestHandle handleFactory) (TestProjectCreate projCreate) =
  let
    idQuery :: Postgres.Query
    idQuery = toQuery $ Filter "id = ?"
  in
    monadicIO $ do
    handle <- run handleFactory
    user <- run User.getUser
    [inserted] <- run $ Project.createProject handle projCreate user
    [fetched] <- run $ Project.getProject handle (Project.id inserted)
    assert $
      (Project.description fetched == Project.description inserted)
