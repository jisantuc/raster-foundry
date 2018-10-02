package com.azavea.rf.api

import akka.http.scaladsl.model.HttpMethods._
import com.azavea.rf.api.healthcheck._
import com.azavea.rf.api.project.ProjectRoutes
import com.azavea.rf.api.user.UserRoutes
import com.azavea.rf.api.utils.Config
import ch.megard.akka.http.cors.scaladsl.CorsDirectives._
import ch.megard.akka.http.cors.scaladsl.settings._

import scala.collection.immutable.Seq

/**
  * Contains all routes for Raster Foundry API/Healthcheck endpoints.
  *
  * Actual routes should be written in the relevant feature as much as is feasible
  *
  */
trait Router
    extends HealthCheckRoutes
    with UserRoutes
    with ProjectRoutes
    with Config {

  val settings = CorsSettings.defaultSettings.copy(
    allowedMethods = Seq(GET, POST, PUT, HEAD, OPTIONS, DELETE))

  val routes = cors(settings) {
    pathPrefix("healthcheck") {
      healthCheckRoutes
    } ~
      pathPrefix("api") {
        pathPrefix("projects") {
          projectRoutes
        } ~
          pathPrefix("users") {
            userRoutes
          }
      }
  }
}
