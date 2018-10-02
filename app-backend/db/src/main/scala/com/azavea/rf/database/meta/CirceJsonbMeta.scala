package com.azavea.rf.database.meta

import doobie._
import doobie.implicits._
import doobie.postgres._
import doobie.postgres.implicits._
import doobie.util.invariant.InvalidObjectMapping
import cats._
import cats.data._
import cats.effect.IO
import cats.syntax.either._
import com.azavea.rf.datamodel._
import io.circe._
import io.circe.syntax._
import org.postgresql.util.PGobject
import com.azavea.rf.datamodel.ColorCorrect._
import com.azavea.rf.datamodel.Credential
import java.net.URI

trait CirceJsonbMeta {
  implicit val credentialMeta: Meta[Credential] =
    Meta
      .other[String]("text")
      .xmap[Credential](
        a => {
          Credential.fromString(a)
        },
        a => a.token.getOrElse("")
      )

  implicit val membershipStatusMeta: Meta[MembershipStatus] =
    Meta
      .other[String]("text")
      .xmap[MembershipStatus](
        a => {
          MembershipStatus.fromString(a)
        },
        a => a.toString
      )

  implicit val jsonbMeta: Meta[Json] =
    Meta
      .other[PGobject]("jsonb")
      .xmap[Json](
        a =>
          io.circe.parser
            .parse(a.getValue)
            .leftMap[Json](e => throw e)
            .merge, // failure raises an exception
        a => {
          val o = new PGobject
          o.setType("jsonb")
          o.setValue(a.noSpaces)
          o
        }
      )

  implicit val colorCorrectionMeta: Meta[ColorCorrect.Params] = {
    Meta
      .other[PGobject]("jsonb")
      .xmap[ColorCorrect.Params](
        a =>
          io.circe.parser
            .parse(a.getValue)
            .leftMap[Json](e => throw e)
            .merge
            .as[ColorCorrect.Params] match {
            case Right(p) => p
            case Left(e)  => throw e
        },
        a => {
          val o = new PGobject
          o.setType("jsonb")
          o.setValue(a.asJson.noSpaces)
          o
        }
      )
  }

  implicit val uriMeta: Meta[URI] = {
    Meta
      .other[String]("text")
      .xmap[URI](
        a => new URI(a),
        a => a.toString
      )
  }

  implicit val UserPersonalInfoMeta: Meta[User.PersonalInfo] = {
    Meta
      .other[PGobject]("jsonb")
      .xmap[User.PersonalInfo](
        a =>
          io.circe.parser
            .parse(a.getValue)
            .leftMap[Json](e => throw e)
            .merge
            .as[User.PersonalInfo] match {
            case Right(p) => p
            case Left(e)  => throw e
        },
        a => {
          val o = new PGobject
          o.setType("jsonb")
          o.setValue(a.asJson.noSpaces)
          o
        }
      )
  }
}
