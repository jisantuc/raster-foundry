addCommandAlias("mg", "migrations/run")

addCommandAlias(
  "gitSnapshots",
  ";set version in ThisBuild := git.gitDescribedVersion.value.get + \"-SNAPSHOT\"")

git.gitTagToVersionNumber in ThisBuild := { tag: String =>
  if (tag matches "[0-9]+\\..*") Some(tag)
  else None
}

lazy val commonSettings = Seq(
  // Add the default sonatype repository setting
  publishTo := sonatypePublishTo.value,
  organization := "com.rasterfoundry",
  organizationName := "Raster Foundry",
  organizationHomepage := Some(new URL("https://www.rasterfoundry.com")),
  description := "A platform to find, combine and analyze earth imagery at any scale.",
  cancelable in Global := true,
  scapegoatVersion in ThisBuild := Version.scapegoat,
  scalaVersion in ThisBuild := Version.scala,
  scalacOptions := Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:implicitConversions",
    "-language:reflectiveCalls",
    "-language:higherKinds",
    "-language:postfixOps",
    "-language:existentials",
    "-language:experimental.macros",
    "-Xmax-classfile-name",
    "100",
    "-Ypartial-unification",
    "-Ypatmat-exhaust-depth",
    "100"
  ),
  updateOptions := updateOptions.value.withGigahorse(false),
  externalResolvers := Seq(
    "Geotoolkit Repo" at "http://maven.geotoolkit.org",
    "Open Source Geospatial Foundation Repo" at "http://download.osgeo.org/webdav/geotools/",
    "boundless" at "https://repo.boundlessgeo.com/main/",
    "imageio-ext Repository" at "http://maven.geo-solutions.it",
    DefaultMavenRepository,
    Resolver.sonatypeRepo("snapshots"),
    Resolver.bintrayRepo("azavea", "maven"),
    Resolver.bintrayRepo("lonelyplanet", "maven"),
    Resolver.bintrayRepo("guizmaii", "maven"),
    Resolver.bintrayRepo("kwark", "maven"), // Required for Slick 3.1.1.2, see https://github.com/azavea/raster-foundry/pull/1576
    "locationtech-releases" at "https://repo.locationtech.org/content/groups/releases",
    "locationtech-snapshots" at "https://repo.locationtech.org/content/groups/snapshots",
    Resolver.bintrayRepo("naftoligug", "maven"),
    Classpaths.sbtPluginReleases,
    Opts.resolver.sonatypeReleases,
    Resolver.bintrayIvyRepo("kamon-io", "sbt-plugins"),
    Resolver.file("local", file(Path.userHome.absolutePath + "/.ivy2/local"))(
      Resolver.ivyStylePatterns) // important to pull deps from the local repo
  ),
  shellPrompt := { s =>
    Project.extract(s).currentProject.id + " > "
  },
  addCompilerPlugin(
    "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
)

// Create a new MergeStrategy for aop.xml files
val aopMerge = new sbtassembly.MergeStrategy {
  val name = "aopMerge"
  import scala.xml._
  import scala.xml.dtd._

  def apply(tempDir: File,
            path: String,
            files: Seq[File]): Either[String, Seq[(File, String)]] = {
    val dt = DocType("aspectj",
                     PublicID("-//AspectJ//DTD//EN",
                              "http://www.eclipse.org/aspectj/dtd/aspectj.dtd"),
                     Nil)
    val file = MergeStrategy.createMergeTarget(tempDir, path)
    val xmls: Seq[Elem] = files.map(XML.loadFile)
    val aspectsChildren: Seq[Node] =
      xmls.flatMap(_ \\ "aspectj" \ "aspects" \ "_")
    val weaverChildren: Seq[Node] =
      xmls.flatMap(_ \\ "aspectj" \ "weaver" \ "_")
    val options: String = xmls
      .map(x => (x \\ "aspectj" \ "weaver" \ "@options").text)
      .mkString(" ")
      .trim
    val weaverAttr =
      if (options.isEmpty) Null
      else new UnprefixedAttribute("options", options, Null)
    val aspects =
      new Elem(null, "aspects", Null, TopScope, false, aspectsChildren: _*)
    val weaver =
      new Elem(null, "weaver", weaverAttr, TopScope, false, weaverChildren: _*)
    val aspectj =
      new Elem(null, "aspectj", Null, TopScope, false, aspects, weaver)
    XML.save(file.toString, aspectj, "UTF-8", xmlDecl = false, dt)
    IO.append(file, IO.Newline.getBytes(IO.defaultCharset))
    Right(Seq(file -> path))
  }
}

lazy val apiSettings = commonSettings ++ Seq(
  fork in run := true,
  connectInput in run := true,
  cancelable in Global := true,
  assemblyJarName in assembly := "rf-server.jar",
  assemblyMergeStrategy in assembly := {
    case "reference.conf"                       => MergeStrategy.concat
    case "application.conf"                     => MergeStrategy.concat
    case n if n.startsWith("META-INF/services") => MergeStrategy.concat
    case n if n.endsWith(".SF") || n.endsWith(".RSA") || n.endsWith(".DSA") =>
      MergeStrategy.discard
    case "META-INF/MANIFEST.MF"          => MergeStrategy.discard
    case PathList("META-INF", "aop.xml") => aopMerge
    case _                               => MergeStrategy.first
  },
  resolvers += "Open Source Geospatial Foundation Repo" at "http://download.osgeo.org/webdav/geotools/",
  resolvers += Resolver.bintrayRepo("azavea", "maven"),
  resolvers += Resolver.bintrayRepo("lonelyplanet", "maven"),
  test in assembly := {}
)
lazy val loggingDependencies = List(
  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
  "ch.qos.logback" % "logback-classic" % "1.1.7"
)

lazy val slickDependencies = List(
  Dependencies.slick,
  Dependencies.slickPG,
  Dependencies.slickPGSpray,
  Dependencies.geotrellisSlick
    .exclude("postgresql", "postgresql")
)

lazy val metricsDependencies = List(
  Dependencies.kamonCore,
  Dependencies.kamonStatsd,
  Dependencies.kamonAkkaHttp
)

lazy val dbDependencies = List(
  Dependencies.hikariCP,
  Dependencies.postgres
)

lazy val forkliftDependencies = List(
  Dependencies.scalaforklift,
  Dependencies.slickMigrationAPI
)

lazy val migrationsDependencies =
  dbDependencies ++ forkliftDependencies ++ loggingDependencies

lazy val testDependencies = List(
  Dependencies.scalatest,
  Dependencies.geotrellisRasterTestkit,
  Dependencies.akkatestkit
)

lazy val apiDependencies = dbDependencies ++ migrationsDependencies ++
  testDependencies ++ metricsDependencies ++ Seq(
  Dependencies.akka,
  Dependencies.akkahttp,
  Dependencies.akkaHttpCors,
  Dependencies.akkaCirceJson,
  Dependencies.akkastream,
  Dependencies.akkaSlf4j,
  Dependencies.akkaHttpExtensions,
  Dependencies.ammoniteOps,
  Dependencies.geotrellisSlick,
  Dependencies.findbugAnnotations,
  Dependencies.dropbox,
  Dependencies.awsStsSdk
)

lazy val root = Project("root", file("."))
  .aggregate(api, db, common, migrations, datamodel)
  .settings(commonSettings: _*)

lazy val api = Project("api", file("api"))
  .dependsOn(db,
             datamodel,
             common % "test->test;compile->compile",
             authentication)
  .settings(apiSettings: _*)
  .settings(resolvers += Resolver.bintrayRepo("hseeberger", "maven"))
  .settings({
    libraryDependencies ++= apiDependencies
  })

lazy val common = Project("common", file("common"))
  .dependsOn(datamodel)
  .settings(apiSettings: _*)
  .settings({
    libraryDependencies ++= testDependencies ++ Seq(
      Dependencies.nimbusJose,
      Dependencies.akkaCirceJson,
      Dependencies.findbugAnnotations,
      Dependencies.ammoniteOps,
      Dependencies.chill,
      Dependencies.catsCore,
      Dependencies.awsBatchSdk,
      Dependencies.awsStsSdk,
      Dependencies.rollbar,
      Dependencies.doobiePostgres,
      Dependencies.geotrellisSlick.exclude("postgresql", "postgresql"),
      Dependencies.apacheCommonsEmail
    )
  })

lazy val db = Project("db", file("db"))
  .dependsOn(datamodel % "compile->compile;test->test", common)
  .settings(commonSettings: _*)
  .settings({
    libraryDependencies ++= dbDependencies ++ loggingDependencies ++ Seq(
      Dependencies.scalatest,
      Dependencies.doobieCore,
      Dependencies.doobieHikari,
      Dependencies.doobieSpecs,
      Dependencies.doobieScalatest,
      Dependencies.doobiePostgres,
      "net.postgis" % "postgis-jdbc" % "2.2.1",
      "net.postgis" % "postgis-jdbc-jtsparser" % "2.2.1",
      "org.locationtech.jts" % "jts-core" % "1.15.0",
      "com.lonelyplanet" %% "akka-http-extensions" % "0.4.15",
      Dependencies.geotrellisSlick.exclude("postgresql", "postgresql")
    )
  })

lazy val migrations = Project("migrations", file("migrations"))
  .settings(commonSettings: _*)
  .settings({
    libraryDependencies ++= migrationsDependencies
  })

lazy val datamodel = Project("datamodel", file("datamodel"))
  .settings(commonSettings: _*)
  .settings(resolvers += Resolver.bintrayRepo("azavea", "geotrellis"))
  .settings({
    libraryDependencies ++= loggingDependencies ++ Seq(
      Dependencies.geotrellisSlick % "provided",
      Dependencies.geotrellisVectorTestkit,
      Dependencies.geotrellisRaster,
      Dependencies.geotrellisGeotools,
      Dependencies.geotools,
      Dependencies.circeCore,
      Dependencies.circeGenericExtras,
      Dependencies.circeOptics,
      Dependencies.circeParser,
      Dependencies.akka,
      Dependencies.akkahttp,
      Dependencies.betterFiles,
      Dependencies.scalaCheck,
      Dependencies.circeTest,
      "com.lonelyplanet" %% "akka-http-extensions" % "0.4.15" % "test",
    )
  })

lazy val authentication = Project("authentication", file("authentication"))
  .dependsOn(common, db)
  .settings(commonSettings: _*)
  .settings({
    libraryDependencies ++= Seq(
      Dependencies.nimbusJose,
      Dependencies.akka,
      Dependencies.akkahttp,
      Dependencies.akkaCirceJson
    )
  })
