package com.azavea.rf.database

import java.util.UUID

import cats.implicits._
import com.azavea.rf.database.Implicits._
import com.azavea.rf.database.filter.Filterables
import com.azavea.rf.database.util._
import com.azavea.rf.datamodel._
import com.lonelyplanet.akka.http.extensions.PageRequest
import doobie._
import doobie.implicits._
import doobie.postgres._
import doobie.postgres.implicits._

/**
  * This is abstraction over the listing of arbitrary types from the DB with filters/pagination
  */
abstract class Dao[Model: Composite] extends Filterables {

  val tableName: String

  /** The fragment which holds the associated table's name */
  def tableF = Fragment.const(tableName)

  /** An abstract select statement to be used for constructing queries */
  def selectF: Fragment

  /** Begin construction of a complex, filtered query */
  def query: Dao.QueryBuilder[Model] =
    Dao.QueryBuilder[Model](selectF, tableF, List.empty)
}

object Dao {

  final case class QueryBuilder[Model: Composite](
      selectF: Fragment,
      tableF: Fragment,
      filters: List[Option[Fragment]]) {

    val countF: Fragment = fr"SELECT count(distinct(id)) FROM" ++ tableF
    val deleteF: Fragment = fr"DELETE FROM" ++ tableF
    val existF: Fragment = fr"SELECT 1 FROM" ++ tableF

    /** Add another filter to the query being constructed */
    def filter[M >: Model, T](thing: T)(
        implicit filterable: Filterable[M, T]): QueryBuilder[Model] =
      this.copy(filters = filters ++ filterable.toFilters(thing))

    def filter[M >: Model](thing: Fragment)(
        implicit filterable: Filterable[M, Fragment]): QueryBuilder[Model] =
      thing match {
        case Fragment.empty => this
        case _              => this.copy(filters = filters ++ filterable.toFilters(thing))
      }

    def filter[M >: Model](id: UUID)(
        implicit filterable: Filterable[M, Option[Fragment]])
      : QueryBuilder[Model] = {
      this.copy(filters = filters ++ filterable.toFilters(Some(fr"id = ${id}")))
    }

    def filter[M >: Model](
        fragments: List[Option[Fragment]]): QueryBuilder[Model] = {
      this.copy(filters = filters ::: fragments)
    }

    // This method exists temporarily to stand in for second-tier object authorization
    def ownedBy[M >: Model](user: User, objectId: UUID): QueryBuilder[Model] =
      this.filter(objectId).filter(user)

    def ownedByOrSuperUser[M >: Model](user: User,
                                       objectId: UUID): QueryBuilder[Model] = {
      if (user.isSuperuser) {
        this.filter(objectId)
      } else {
        this.filter(objectId).filter(user)
      }
    }

    def pageOffset[T: Composite](
        pageRequest: PageRequest): ConnectionIO[List[T]] =
      (selectF ++ Fragments.whereAndOpt(filters: _*) ++ Page(pageRequest))
        .query[T]
        .to[List]

    /** Provide a list of responses within the PaginatedResponse wrapper */
    def page[T: Composite](
        pageRequest: PageRequest,
        selectF: Fragment,
        countF: Fragment,
        orderClause: Fragment = fr""): ConnectionIO[PaginatedResponse[T]] = {
      for {
        page <- (selectF ++ Fragments.whereAndOpt(filters: _*) ++ orderClause ++ Page(
          pageRequest)).query[T].to[List]
        count <- (countF ++ Fragments.whereAndOpt(filters: _*))
          .query[Int]
          .unique
      } yield {
        val hasPrevious = pageRequest.offset > 0
        val hasNext = (pageRequest.offset * pageRequest.limit) + 1 < count

        PaginatedResponse[T](count,
                             hasPrevious,
                             hasNext,
                             pageRequest.offset,
                             pageRequest.limit,
                             page)
      }
    }

    /** Provide a list of responses within the PaginatedResponse wrapper */
    def page(pageRequest: PageRequest): ConnectionIO[PaginatedResponse[Model]] =
      page(pageRequest, selectF, countF)

    def listQ(pageRequest: PageRequest): Query0[Model] =
      (selectF ++ Fragments.whereAndOpt(filters: _*) ++ Page(Some(pageRequest)))
        .query[Model]

    /** Provide a list of responses */
    def list(pageRequest: PageRequest): ConnectionIO[List[Model]] = {
      listQ(pageRequest).to[List]
    }

    /** Short circuit for quickly getting an approximate count for large queries (e.g. scenes) **/
    def sceneCountIO: ConnectionIO[Int] = {
      val countQuery = countF ++ Fragments.whereAndOpt(filters: _*)
      val over100IO: ConnectionIO[Boolean] =
        (fr"SELECT EXISTS(" ++ (selectF ++ Fragments.whereAndOpt(filters: _*) ++ fr"offset 100") ++ fr")")
          .query[Boolean]
          .unique
      over100IO flatMap {
        {
          case true  => 100.pure[ConnectionIO]
          case false => countQuery.query[Int].unique
        }
      }
    }

    def listQ(limit: Int): Query0[Model] =
      (selectF ++ Fragments.whereAndOpt(filters: _*) ++ fr"LIMIT $limit")
        .query[Model]

    /** Provide a list of responses */
    def list(limit: Int): ConnectionIO[List[Model]] = {
      listQ(limit).to[List]
    }

    def listQ(offset: Int, limit: Int): Query0[Model] =
      (selectF ++ Fragments.whereAndOpt(filters: _*) ++ fr"OFFSET $offset" ++ fr"LIMIT $limit")
        .query[Model]

    def listQ(offset: Int, limit: Int, orderClause: Fragment): Query0[Model] =
      (selectF ++ Fragments.whereAndOpt(filters: _*) ++ orderClause ++ fr"OFFSET $offset" ++ fr"LIMIT $limit")
        .query[Model]

    /** Provide a list of responses */
    def list: ConnectionIO[List[Model]] = {
      (selectF ++ Fragments.whereAndOpt(filters: _*))
        .query[Model]
        .to[List]
    }

    /** Provide a list of responses */
    def list(offset: Int, limit: Int): ConnectionIO[List[Model]] = {
      listQ(offset, limit).to[List]
    }

    def list(offset: Int,
             limit: Int,
             orderClause: Fragment): ConnectionIO[List[Model]] = {
      listQ(offset, limit, orderClause).to[List]
    }

    def selectQ: Query0[Model] =
      (selectF ++ Fragments.whereAndOpt(filters: _*)).query[Model]

    /** Select a single value - returning an Optional value */
    def selectOption: ConnectionIO[Option[Model]] =
      selectQ.option

    /** Select a single value - throw on failure */
    def select: ConnectionIO[Model] = {
      selectQ.unique
    }

    def deleteQOption: Option[Update0] = {
      if (filters.isEmpty) {
        None
      } else {
        Some((deleteF ++ Fragments.whereAndOpt(filters: _*)).update)
      }
    }

    def delete: ConnectionIO[Int] = {
      deleteQOption
        .getOrElse(
          throw new Exception("Unsafe delete - delete requires filters"))
        .run
    }

    def exists: ConnectionIO[Boolean] = {
      (existF ++ Fragments.whereAndOpt(filters: _*) ++ fr"LIMIT 1")
        .query[Int]
        .to[List]
        .map(_.nonEmpty)
    }

  }
}
