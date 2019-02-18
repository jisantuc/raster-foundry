module Database.RasterFoundry.Project where

import           Data.RasterFoundry.Classes.Filter   (Filterable(..), toQuery)
import           Data.RasterFoundry.Types.Project
import           Data.RasterFoundry.Types.Time       (now)
import qualified Data.RasterFoundry.Types.User    as User
import qualified Database.RasterFoundry           as Database

import qualified Database.PostgreSQL.Simple       as Postgres
import           Data.UUID                           (UUID)
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Int                             (Int64)

-- choose a default layer id at random from the layers that happen to be in my dev database
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
      single_band_options,
      default_layer_id
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, '544e4ba7-6679-4d2d-bc90-8aaac7ce0502')
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
  FROM projects WHERE id = ?; |]

listQuery :: Postgres.Query
listQuery =
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
  FROM projects |]

deleteQuery :: Postgres.Query
deleteQuery =
  [sql|DELETE FROM projects
      where id = ?; |]

updateQuery :: Postgres.Query
updateQuery =
  [sql| UPDATE projects
      SET
        modified_at = ?,
        modified_by = ?,
        name = ?,
        slug_label = ?,
        description = ?,
        visibility = ?,
        tags = ?,
        manual_order = ?,
        tile_visibility = ?,
        is_aoi_project = ?,
        aoi_cadence_millis = ?,
        aois_last_checked = ?,
        is_single_band = ?,
        single_band_options = ?
      WHERE
        id = ?; |]

{- This pretends to do some work to update aois but really just sets the last checked time
-}
checkAOIs :: Project -> IO Project
checkAOIs project = do
  modifiedTime <- now
  return $ project { modifiedAt = modifiedTime
                   , aoisLastChecked = modifiedTime }

createProject :: Database.Handle -> Create -> User.User -> IO [Project]
createProject handle projectCreate user = do
  project <- toProject projectCreate (User.id user)
  Postgres.query (Database.conn handle) insertQuery project

getProject :: Database.Handle -> UUID -> IO [Project]
getProject handle projectId =
  Postgres.query (Database.conn handle) getQuery (Postgres.Only projectId)

listProjects :: Filterable a => Database.Handle -> a -> IO [Project]
listProjects handle filt =
  Postgres.query_ (Database.conn handle) (listQuery <> " WHERE " <> (toQuery . toFilter $ filt))

deleteProject :: Database.Handle -> UUID -> IO Int64
deleteProject handle projectId =
  Postgres.execute (Database.conn handle) deleteQuery (Postgres.Only projectId)

updateProject :: Database.Handle -> Project -> User.User -> IO Int64
updateProject handle project user = do
  projectUpdate <- toUpdate project (User.id user)
  Postgres.execute (Database.conn handle) updateQuery projectUpdate
