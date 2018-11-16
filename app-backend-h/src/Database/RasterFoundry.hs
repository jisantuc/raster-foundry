module Database.RasterFoundry where

import Database.PostgreSQL.Simple as Postgres

data Handle = Handle { conn :: Postgres.Connection }
