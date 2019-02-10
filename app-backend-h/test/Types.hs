module Types where

import Data.Bifunctor (bimap)
import Test.QuickCheck (Arbitrary(..), elements, arbitraryBoundedIntegral)
import Test.QuickCheck.Instances.Time
import Test.QuickCheck.Instances.UUID
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (toJSON)
import qualified Data.Map as M
import Database.PostgreSQL.Simple.Time (Unbounded(..))
import Data.RasterFoundry.Types.Project
import Data.RasterFoundry.Types.Time
import Data.RasterFoundry.Types.Visibility
import qualified Database.RasterFoundry as Database
import qualified Database.PostgreSQL.Simple           as Postgres
import qualified Database.PostgreSQL.Simple.Types     as Postgres
import GHC.Generics (Generic)

import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUIDv4

newtype TestHandle = TestHandle (IO Database.Handle)
instance Arbitrary TestHandle where
  arbitrary =
    let
      connString =
        "postgresql://rasterfoundry:rasterfoundry@localhost:5432/rasterfoundry"
    in
      pure . TestHandle $ do
        conn <- Postgres.connectPostgreSQL connString
        pure $ Database.Handle conn

instance Show TestHandle where
  show _ = "test db handle"

newtype WebMercatorX = WebMercatorX Int deriving (Eq, Show, Ord)
instance Enum WebMercatorX where
  toEnum x = (toEnum x)
  fromEnum (WebMercatorX x) = fromEnum x

instance Num WebMercatorX where
  (+) (WebMercatorX x1) (WebMercatorX x2) = WebMercatorX (x1 + x2)
  (*) (WebMercatorX x1) (WebMercatorX x2) = WebMercatorX (x1 * x2)
  (-) (WebMercatorX x1) (WebMercatorX x2) = WebMercatorX (x1 - x2)
  abs (WebMercatorX x1) = WebMercatorX (abs x1)
  negate (WebMercatorX x1) = WebMercatorX (negate x1)
  signum (WebMercatorX x) = WebMercatorX (signum x)
  fromInteger i = WebMercatorX (fromInteger i)

instance Real WebMercatorX where
  toRational (WebMercatorX x) = toRational x
instance Integral WebMercatorX where
  toInteger (WebMercatorX x) = toInteger x
  quotRem (WebMercatorX x1) (WebMercatorX x2) =
    bimap WebMercatorX WebMercatorX $ quotRem x1 x2
instance Bounded WebMercatorX where
   minBound = WebMercatorX (negate 20026376)
   maxBound = WebMercatorX 20026376

newtype WebMercatorY = WebMercatorY Int deriving (Eq, Show, Ord)
instance Bounded WebMercatorY where
  minBound = WebMercatorY (negate 20048966)
  maxBound = WebMercatorY 20048966
instance Enum WebMercatorY where
  toEnum y = (toEnum y)
  fromEnum (WebMercatorY y) = fromEnum y
instance Num WebMercatorY where
  (+) (WebMercatorY y1) (WebMercatorY y2) = WebMercatorY (y1 + y2)
  (*) (WebMercatorY y1) (WebMercatorY y2) = WebMercatorY (y1 * y2)
  (-) (WebMercatorY y1) (WebMercatorY y2) = WebMercatorY (y1 - y2)
  abs (WebMercatorY y1) = WebMercatorY (abs y1)
  negate (WebMercatorY y1) = WebMercatorY (negate y1)
  signum (WebMercatorY y) = WebMercatorY (signum y)
  fromInteger i = WebMercatorY (fromInteger i)
instance Real WebMercatorY where
  toRational (WebMercatorY y) = toRational y
instance Integral WebMercatorY where
  toInteger (WebMercatorY y) = toInteger y
  quotRem (WebMercatorY y1) (WebMercatorY y2) =
    bimap WebMercatorY WebMercatorY $ quotRem y1 y2

newtype TestGeometry = TestGeometry Geometry deriving (Eq, Show, Generic)
instance Arbitrary TestGeometry where
  arbitrary = do
    (WebMercatorX llX) <- arbitraryBoundedIntegral
    (WebMercatorY llY) <- arbitraryBoundedIntegral
    pure . TestGeometry $
      Polygon1 [[ (llX, llY)
                , (llX + 500, llY)
                , (llX + 500, llY + 500)
                , (llX, llY + 500)
                , (llX, llY)]] Polygon

newtype TestSqlTime = TestSqlTime SqlTime deriving (Eq, Show, Generic)
instance Arbitrary TestSqlTime where
  arbitrary = TestSqlTime . SqlTime . Finite <$> arbitrary

newtype TestVisibility =
  TestVisibility Visibility deriving (Eq, Show, Generic)
instance Arbitrary TestVisibility where
  arbitrary = TestVisibility <$> elements [Public, Private]

newtype TestPGArray a = TestPGArray (Postgres.PGArray a)
instance (Arbitrary a) => Arbitrary (TestPGArray a) where
  arbitrary = TestPGArray <$>
    do
      arr <- arbitrary
      return $ Postgres.PGArray arr

newtype TestProject =
  TestProject Project deriving (Eq, Show, Generic)

instance Arbitrary TestProject where
  arbitrary = TestProject <$>
    do
      _id <- arbitrary
      (TestSqlTime _createdAt) <- arbitrary
      (TestSqlTime _modifiedAt) <- arbitrary
      _createdBy <- arbitrary
      _modifiedBy <- arbitrary
      _name <- arbitrary
      _slugLabel <- arbitrary
      _description <- arbitrary
      (TestVisibility _visibility) <- arbitrary
      (TestPGArray _tags) <- arbitrary
      _manualOrder <- arbitrary
      (TestGeometry _extent) <- arbitrary
      (TestVisibility _tileVisibility) <- arbitrary
      _isAOIProject <- arbitrary
      _aoiCadenceMillis <- arbitrary
      (TestSqlTime _aoisLastChecked) <- arbitrary
      _owner <- arbitrary
      _isSingleBand <- arbitrary
      _singleBandOptions <- if (_isSingleBand) then
        pure . SingleBandOptions <$> arbitrary else
        pure Nothing
      return $
        Project _id _createdAt _modifiedAt _createdBy
        _modifiedBy _name _slugLabel _description _visibility
        _tags _manualOrder (Just _extent) _tileVisibility
        _isAOIProject _aoiCadenceMillis
        _aoisLastChecked _owner _isSingleBand _singleBandOptions

newtype TestProjectCreate = TestProjectCreate Create deriving (Show)
instance Arbitrary TestProjectCreate where
  arbitrary = TestProjectCreate <$>
    do
      _name <- arbitrary
      _description <- arbitrary
      _isAOIProject <- arbitrary
      _aoiCadenceMillis <- arbitrary
      _owner <- arbitrary
      (TestPGArray _tags) <- arbitrary
      _isSingleBand <- arbitrary
      _singleBandOptions <- if (_isSingleBand) then
        pure . SingleBandOptions <$> arbitrary else
        pure Nothing
      _extras <- pure . toJSON $ (M.empty :: M.Map String Int)
      pure $ Create _name _description _isAOIProject _aoiCadenceMillis _owner _tags _isSingleBand _singleBandOptions _extras
