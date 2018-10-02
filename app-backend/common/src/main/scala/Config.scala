package com.azavea.rf.common

import java.util.concurrent.TimeUnit

import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._

object Config {
  private val config = ConfigFactory.load()

  object awsbatch {
    private val awsBatchConfig = config.getConfig("awsbatch")
    val jobQueue = awsBatchConfig.getString("jobQueue")
    val ingestJobQueue = awsBatchConfig.getString("ingestJobQueue")

    val ingestJobName = awsBatchConfig.getString("ingestJobName")
    val importJobName = awsBatchConfig.getString("importJobName")
    val exportJobName = awsBatchConfig.getString("exportJobName")
    val aoiUpdateJobName = awsBatchConfig.getString("aoiUpdateJobName")

    val environment = awsBatchConfig.getString("environment")
  }

  object geotrellis {
    private lazy val geotrellisConfig = config.getConfig("geotrellis")

    lazy val postgresAttributeStoreThreads: Int =
      geotrellisConfig.getInt("attributeStore.postgres.threads")

    lazy val postgresAttributeStoreTimeout: FiniteDuration =
      FiniteDuration(
        geotrellisConfig.getDuration("attributeStore.postgres.timeout").toNanos,
        TimeUnit.NANOSECONDS)
  }

}
