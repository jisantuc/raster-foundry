package com.rasterfoundry.database

import com.rasterfoundry.datamodel._
import com.rasterfoundry.database.Implicits._

import doobie._, doobie.implicits._
import cats._, cats.data._, cats.effect.IO
import cats.syntax.either._
import doobie.postgres._
import doobie.postgres.implicits._
import doobie.postgres.circe.jsonb.implicits._
import doobie.scalatest.imports._
import org.scalatest._

class ToolDaoSpec
    extends FunSuite
    with Matchers
    with IOChecker
    with DBTestConfig {
  test("selection types") { check(ToolDao.selectF.query[Tool]) }
}
