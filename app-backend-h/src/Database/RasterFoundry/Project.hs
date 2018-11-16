module Database.RasterFoundry.Project where

import Data.RasterFoundry.Types.Project
import qualified Data.RasterFoundry.Types.User    as User
import qualified Database.RasterFoundry           as Database

import qualified Database.PostgreSQL.Simple       as Postgres
import           Data.UUID                            (UUID)
import Database.PostgreSQL.Simple.SqlQQ

insertQuery :: Postgres.Query
insertQuery =
  [sql|INSERT INTO projects (
      id,
      created_at,
      modified_at,
      created_by,
      modified_by,
      name,
      slug_label,
      description,
      visibility,
      tags,
      manual_order,
      extent,
      tile_visibility,
      is_aoi_project,
      aoi_cadence_millis,
      aois_last_checked,
      owner,
      is_single_band,
      single_band_options
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    RETURNING id,
    created_at,
    modified_at,
    created_by,
    modified_by,
    name,
    slug_label,
    description,
    visibility,
    tags,
    manual_order,
    extent,
    tile_visibility,
    is_aoi_project,
    aoi_cadence_millis,
    aois_last_checked,
    owner,
    is_single_band,
    single_band_options; |]

getQuery :: Postgres.Query
getQuery =
  [sql|SELECT
      id,
      created_at,
      modified_at,
      created_by,
      modified_by,
      name,
      slug_label,
      description,
      visibility,
      tags,
      manual_order,
      extent,
      tile_visibility,
      is_aoi_project,
      aoi_cadence_millis,
      aois_last_checked,
      owner,
      is_single_band,
      single_band_options
  FROM projects where id = ?; |]

createProject :: Database.Handle -> Create -> User.User -> IO [Project]
createProject handle projectCreate user = do
  project <- toProject projectCreate (User.id user)
  Postgres.query (Database.conn handle) insertQuery project

getProject :: Database.Handle -> UUID -> IO [Project]
getProject handle projectId =
  Postgres.query (Database.conn handle) getQuery (Postgres.Only projectId)
