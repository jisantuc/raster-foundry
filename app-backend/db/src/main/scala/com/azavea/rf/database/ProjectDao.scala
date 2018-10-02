package com.azavea.rf.database

import java.sql.Timestamp
import java.util.UUID

import cats.data._
import cats.implicits._
import com.azavea.rf.database.util.Page
import com.azavea.rf.database.Implicits._
import com.azavea.rf.datamodel._
import com.azavea.rf.datamodel.color._
import com.lonelyplanet.akka.http.extensions.PageRequest
import doobie._
import doobie.implicits._
import doobie.postgres._
import doobie.postgres.implicits._
import io.circe._
import io.circe.optics.JsonPath._
import io.circe.syntax._

object ProjectDao extends Dao[Project] {

  val tableName = "projects"

  val selectF: Fragment = sql"""
    SELECT
      distinct(id), created_at, modified_at, created_by,
      modified_by, owner, name, slug_label, description,
      visibility, tile_visibility, is_aoi_project,
      aoi_cadence_millis, aois_last_checked, tags, extent,
      manual_order, is_single_band, single_band_options,
      default_annotation_group, extras
    FROM
  """ ++ tableF

  def unsafeGetProjectById(projectId: UUID): ConnectionIO[Project] = {
    val idFilter = Some(fr"id = ${projectId}")

    (selectF ++ Fragments.whereAndOpt(idFilter))
      .query[Project]
      .unique
  }

  def getProjectById(projectId: UUID): ConnectionIO[Option[Project]] = {
    val idFilter = Some(fr"id = ${projectId}")

    (selectF ++ Fragments.whereAndOpt(idFilter))
      .query[Project]
      .option
  }

  def listProjects(
      page: PageRequest,
      params: ProjectQueryParameters,
      user: User): ConnectionIO[PaginatedResponse[Project]] =
    query.filter(user).filter(params).page(page)

  def isProjectPublic(projectId: UUID): ConnectionIO[Boolean] = {
    this.query
      .filter(projectId)
      .filter(fr"visibility = 'PUBLIC'")
      .exists
  }

  def insertProject(newProject: Project.Create,
                    user: User): ConnectionIO[Project] = {
    val id = UUID.randomUUID()
    val now = new Timestamp(new java.util.Date().getTime)
    val ownerId = util.Ownership.checkOwner(user, newProject.owner)
    val slug = Project.slugify(newProject.name)
    (fr"INSERT INTO" ++ tableF ++ fr"""
        (id, created_at, modified_at, created_by,
        modified_by, owner, name, slug_label, description,
        visibility, tile_visibility, is_aoi_project,
        aoi_cadence_millis, aois_last_checked, tags, extent,
        manual_order, is_single_band, single_band_options, default_annotation_group,
        extras)
      VALUES
        ($id, $now, $now, ${user.id},
        ${user.id}, $ownerId, ${newProject.name}, $slug, ${newProject.description},
        ${newProject.visibility}, ${newProject.tileVisibility}, ${newProject.isAOIProject},
        ${newProject.aoiCadenceMillis}, $now, ${newProject.tags}, null,
        TRUE, ${newProject.isSingleBand}, ${newProject.singleBandOptions}, null,
        ${newProject.extras}
      )
    """).update.withUniqueGeneratedKeys[Project](
      "id",
      "created_at",
      "modified_at",
      "created_by",
      "modified_by",
      "owner",
      "name",
      "slug_label",
      "description",
      "visibility",
      "tile_visibility",
      "is_aoi_project",
      "aoi_cadence_millis",
      "aois_last_checked",
      "tags",
      "extent",
      "manual_order",
      "is_single_band",
      "single_band_options",
      "default_annotation_group",
      "extras"
    )
  }

  def updateProjectQ(project: Project, id: UUID, user: User): Update0 = {
    val updateTime = new Timestamp(new java.util.Date().getTime)
    val idFilter = fr"id = ${id}"

    val query = (fr"UPDATE" ++ tableF ++ fr"""SET
       modified_at = ${updateTime},
       modified_by = ${user.id},
       owner = ${project.owner},
       name = ${project.name},
       description = ${project.description},
       visibility = ${project.visibility},
       tile_visibility = ${project.tileVisibility},
       is_aoi_project = ${project.isAOIProject},
       aoi_cadence_millis = ${project.aoiCadenceMillis},
       aois_last_checked = ${project.aoisLastChecked},
       tags = ${project.tags},
       extent = ${project.extent},
       manual_order = ${project.manualOrder},
       is_single_band = ${project.isSingleBand},
       single_band_options = ${project.singleBandOptions},
       default_annotation_group = ${project.defaultAnnotationGroup},
       extras = ${project.extras}
    """ ++ Fragments.whereAndOpt(Some(idFilter))).update
    query
  }

  def updateProject(project: Project,
                    id: UUID,
                    user: User): ConnectionIO[Int] = {
    updateProjectQ(project, id, user).run
  }

  def deleteProject(id: UUID): ConnectionIO[Int] = {

    val aoiDeleteQuery = sql"DELETE FROM aois where aois.project_id = ${id}"
    for {
      _ <- aoiDeleteQuery.update.run
      projectDeleteCount <- query.filter(fr"id = ${id}").delete
    } yield projectDeleteCount
  }

  // head is safe here, because we're looking up users from the ids in projects, and the map was
  // build from those same ids.
  // throwing the exception is also safe, since the foreign key from project owners to users requires
  // that every project's owner is a key in the resulting list of users
  @SuppressWarnings(Array("TraversableHead"))
  def projectsToProjectsWithRelated(projectsPage: PaginatedResponse[Project])
    : ConnectionIO[PaginatedResponse[Project.WithUser]] =
    projectsPage.results.toList.toNel match {
      case Some(nelProjects) =>
        val usersIO: ConnectionIO[List[User]] =
          UserDao.query
            .filter(Fragments.in(fr"id", nelProjects map { _.owner }))
            .list
        usersIO map { users: List[User] =>
          {
            val groupedUsers = users.groupBy(_.id)
            val withUsers =
              projectsPage.results map { project: Project =>
                Project.WithUser(
                  project,
                  groupedUsers
                    .getOrElse(
                      project.owner,
                      throw new Exception(
                        "Somehow, a user id was lost to the aether")
                    )
                    .head
                )
              }
            projectsPage.copy(results = withUsers)
          }
        }
      case _ =>
        projectsPage
          .copy(results = List.empty[Project.WithUser])
          .pure[ConnectionIO]
    }
}
