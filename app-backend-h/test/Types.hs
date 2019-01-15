module Types where

import Test.QuickCheck (Arbitrary(..), elements)
import Test.QuickCheck.Instances.UUID
import Control.Monad.IO.Class (MonadIO(..))
import Data.RasterFoundry.Types.Project
import Data.RasterFoundry.Types.Visibility
import qualified Database.PostgreSQL.Simple.Types     as Postgres
import GHC.Generics (Generic)

import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUIDv4

newtype TestProject =
  TestProject Project deriving (Eq, Show, Generic)

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

instance Arbitrary TestProject where
  arbitrary = TestProject <$>
    do
      _id <- arbitrary
      _createdAt <- arbitrary
      _modifiedAt <- arbitrary
      _createdBy <- arbitrary
      _modifiedBy <- arbitrary
      _name <- arbitrary
      _slugLabel <- arbitrary
      _description <- arbitrary
      (TestVisibility _visibility) <- arbitrary
      (TestPGArray _tags) <- arbitrary
      _manualOrder <- arbitrary
      _extent <- arbitrary
      (TestVisibility _tileVisibility) <- arbitrary
      _isAOIProject <- arbitrary
      _aoiCadenceMillis <- arbitrary
      _aoisLastChecked <- arbitrary
      _owner <- arbitrary
      _isSingleBand <- arbitrary
      _singleBandOptions <- arbitrary
      return $
        Project _id _createdAt _modifiedAt _createdBy _modifiedBy _name _slugLabel _description _visibility _tags _manualOrder _extent _tileVisibility _isAOIProject _aoiCadenceMillis _aoisLastChecked _owner _isSingleBand _singleBandOptions
