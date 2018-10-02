package com.azavea.rf.api.project

import java.util.{Calendar, UUID}

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.FileInfo
import better.files.{File => ScalaFile}
import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._
import com.azavea.rf.api.utils.Config
import com.azavea.rf.api.utils.queryparams.QueryParametersCommon
import com.azavea.rf.authentication.Authentication
import com.azavea.rf.common.utils.Shapefile
import com.azavea.rf.common.{
  CommonHandlers,
  UserErrorHandler
}
import com.azavea.rf.database._
import com.azavea.rf.database.filter.Filterables._
import com.azavea.rf.datamodel._
import com.lonelyplanet.akka.http.extensions.{PageRequest, PaginationDirectives}
import com.typesafe.scalalogging.LazyLogging
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._
import doobie._
import doobie.implicits._
import doobie.postgres.implicits._
import doobie.util.transactor.Transactor
import io.circe.generic.JsonCodec
import kamon.akka.http.KamonTraceDirectives

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

trait ProjectRoutes
    extends Authentication
    with Config
    with QueryParametersCommon
    with PaginationDirectives
    with CommonHandlers
    with UserErrorHandler
    with KamonTraceDirectives
    with LazyLogging {

  val xa: Transactor[IO]

  val BULK_OPERATION_MAX_LIMIT = 100

  val projectRoutes: Route = handleExceptions(userExceptionHandler) {
    pathEndOrSingleSlash {
      get {
        traceName("projects-list") {
          listProjects
        }
      } ~
        post {
          traceName("projects-create") {
            createProject
          }
        }
    } ~
      pathPrefix(JavaUUID) { projectId =>
        pathEndOrSingleSlash {
          get {
            traceName("projects-detail") {
              getProject(projectId)
            }
          } ~
            put {
              traceName("projects-update") {
                updateProject(projectId)
              }
            } ~
            delete {
              traceName("projects-delete") {
                deleteProject(projectId)
              }
            }
        }
      }
  }

  def listProjects: Route = authenticate { user =>
    (withPagination & projectQueryParameters) {
      (page, projectQueryParameters) =>
        complete {
          ProjectDao
            .listProjects(page, projectQueryParameters, user)
            .transact(xa)
            .unsafeToFuture
        }
    }
  }

  def createProject: Route = authenticate { user =>
    entity(as[Project.Create]) { newProject =>
      onSuccess(
        ProjectDao
          .insertProject(newProject, user)
          .transact(xa)
          .unsafeToFuture) { project =>
        complete(StatusCodes.Created, project)
      }
    }
  }

  def getProject(projectId: UUID): Route = {
    onComplete(
      ProjectDao.isProjectPublic(projectId).transact(xa).unsafeToFuture
    ) {
      case Success(true) =>
        rejectEmptyResponse {
          complete {
            ProjectDao.query
              .filter(projectId)
              .selectOption
              .transact(xa)
              .unsafeToFuture
          }
        }
      case _ =>
        authenticate { user =>
          authorizeAsync {
            ProjectDao.query
              .ownedBy(user, projectId)
              .exists
              .transact(xa)
              .unsafeToFuture
          } {
            rejectEmptyResponse {
              complete {
                ProjectDao.query
                  .filter(projectId)
                  .selectOption
                  .transact(xa)
                  .unsafeToFuture
              }
            }
          }
        }
    }
  }

  def updateProject(projectId: UUID): Route = authenticate { user =>
    authorizeAsync {
      ProjectDao.query
        .ownedBy(user, projectId)
        .exists
        .transact(xa)
        .unsafeToFuture
    } {
      entity(as[Project]) { updatedProject =>
        onSuccess(
          ProjectDao
            .updateProject(updatedProject, projectId, user)
            .transact(xa)
            .unsafeToFuture) {
          completeSingleOrNotFound
        }
      }
    }
  }

  def deleteProject(projectId: UUID): Route = authenticate { user =>
    authorizeAsync {
      ProjectDao.query
        .ownedBy(user, projectId)
        .exists
        .transact(xa)
        .unsafeToFuture
    } {
      onSuccess(ProjectDao.deleteProject(projectId).transact(xa).unsafeToFuture) {
        completeSingleOrNotFound
      }
    }
  }

}
