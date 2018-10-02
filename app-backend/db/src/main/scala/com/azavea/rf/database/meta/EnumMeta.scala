package com.azavea.rf.database.meta

import com.azavea.rf.datamodel._

import doobie._, doobie.implicits._
import doobie.postgres._, doobie.postgres.implicits._
import doobie.util.invariant.InvalidObjectMapping
import cats._, cats.data._, cats.effect.IO

trait EnumMeta {
  implicit val visibilityMeta: Meta[Visibility] =
    pgEnumString("visibility", Visibility.fromString, _.repr)

  implicit val userRoleMeta: Meta[UserRole] =
    pgEnumString("user_role", UserRole.fromString, _.repr)

  implicit val groupTypeMeta: Meta[GroupType] =
    pgEnumString("group_type", GroupType.fromString, _.repr)

  implicit val subjectTypeMeta: Meta[SubjectType] =
    pgEnumString("subject_type", SubjectType.fromString, _.repr)

  implicit val objectTypeMeta: Meta[ObjectType] =
    pgEnumString("object_type", ObjectType.fromString, _.repr)

  implicit val userVisibilityMeta: Meta[UserVisibility] =
    pgEnumString("user_visibility", UserVisibility.fromString, _.repr)
}
