package com.azavea.rf.authentication

import java.net.URL
import java.util.UUID

import akka.http.scaladsl.model.headers.HttpChallenge
import akka.http.scaladsl.server.AuthenticationFailedRejection.CredentialsRejected
import akka.http.scaladsl.server._
import cats.effect.IO
import cats.implicits._
import com.azavea.rf.database._
import com.azavea.rf.datamodel._
import com.guizmaii.scalajwt.{ConfigurableJwtValidator, JwtToken}
import com.nimbusds.jose.jwk.source.{JWKSource, RemoteJWKSet}
import com.nimbusds.jose.proc.SecurityContext
import com.nimbusds.jwt.JWTClaimsSet
import com.nimbusds.jwt.proc.BadJWTException
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import doobie._
import doobie.implicits._
import doobie.postgres.implicits._
import doobie.util.transactor.Transactor

import scala.concurrent.Future

trait Authentication extends Directives with LazyLogging {

  implicit def xa: Transactor[IO]

  // HTTP Challenge to use for Authentication failures
  lazy val challenge = HttpChallenge("Bearer", "https://rasterfoundry.com")

  /**
    * Helper directive to extract token header
    */
  def extractTokenHeader: Directive1[String] = {
    optionalHeaderValueByName("Authorization").flatMap {
      case Some(tokenString) => provide(tokenString.split(" ").last)
      case _ => reject(AuthenticationFailedRejection(CredentialsRejected, challenge))
    }
  }

  /**
       * Authenticates user based on bearer token (JWT)
       */
  def authenticate: Directive1[User] = {
    extractTokenHeader.flatMap { token =>
      authenticateWithToken(token)
    }
  }

  @SuppressWarnings(Array("TraversableHead"))
  def authenticateWithToken(tokenString: String): Directive1[User] = {
    tokenString match {
      case "" => reject(AuthenticationFailedRejection(AuthenticationFailedRejection.CredentialsMissing,
                                                      challenge))
      case s => provide(User.Create(tokenString).toUser)
    }
  }
}
