import slick.jdbc.PostgresProfile.api._
import com.liyaos.forklift.slick.SqlMigration

object M125 {
  RFMigrations.migrations = RFMigrations.migrations :+ SqlMigration(125)(
    List(
      sqlu"""
ALTER TABLE uploads ALTER COLUMN upload_type TYPE varchar(255);
DROP TYPE upload_type;
CREATE TYPE upload_type AS ENUM ('DROPBOX', 'S3', 'LOCAL', 'PLANET', 'MODIS_USGS', 'LANDSAT_HISTORICAL');
ALTER TABLE uploads ALTER COLUMN upload_type TYPE upload_type using upload_type::upload_type;
"""
    ))
}
