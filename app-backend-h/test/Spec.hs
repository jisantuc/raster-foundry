import qualified Database.PostgreSQL.Simple.Types as Postgres
import qualified Database.RasterFoundry as Database
import qualified Database.RasterFoundry.Project as Project
import           Data.ByteString                   (ByteString, pack)
import           Data.RasterFoundry.Classes.Filter
import qualified Data.RasterFoundry.Types.Project as Project
import qualified Data.RasterFoundry.Types.User as User

import Types
import Test.QuickCheck
import Test.QuickCheck.Monadic

main :: IO ()
main = do
  quickCheck insertRoundTripProp
  quickCheck getProjectProp

connString :: ByteString
connString = "postgresql://rasterfoundry:rasterfoundry@localhost:5432/rasterfoundry"

prop_3s :: Property
prop_3s = monadicIO $ do
  three1 <- pure 3
  three2 <- pure 3
  assert $ three1 == three2

insertRoundTripProp :: TestHandle -> TestProjectCreate -> Property
insertRoundTripProp (TestHandle handleFactory) (TestProjectCreate projCreate) = monadicIO $ do
  handle <- run handleFactory
  user <- run User.getUser
  [inserted] <- run $ Project.createProject handle projCreate user
  assert $ Project.description inserted == Project._description projCreate

getProjectProp :: TestHandle -> TestProjectCreate -> Property
getProjectProp (TestHandle handleFactory) (TestProjectCreate projCreate) =
  let
    idQuery :: ByteString -> Postgres.Query
    idQuery someId = pack "id = " ++ someId
  in
    monadicIO $ do
    handle <- run handleFactory
    user <- run User.getUser
    [inserted] <- run $ Project.createProject handle projCreate user
    [fetched] <- run $ Project.getProject handle (Project.id inserted)
    [listed] <- run $ Project.listProjects handle (toQuery . orFilter $ idFilter (pack . show $ Project.id inserted))
    assert $
      (Project.description fetched == Project.description inserted) &&
      (Project.description fetched == Project.description listed)
