import sbt._
import Keys._
import com.typesafe.sbt.osgi.SbtOsgi._

object BuildSettings {
  val sv = "2.10.0"
  val buildOrganization = "dire"
  val buildVersion = "0.1.0-SNAPSHOT"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := sv,
    publishTo := Some(Resolver.file("file", 
      new File(Path.userHome.absolutePath+"/.m2/repository"))),
    scalacOptions ++= Seq ("-deprecation", "-feature",
      "-language:postfixOps", "-language:implicitConversions",
      "-language:higherKinds")
  ) ++ osgiSettings
} 

object Dependencies {
  val scalaz = "org.scalaz"
  val scalazV = "7.0.0-M8"

  val scalaz_core = scalaz %% "scalaz-core" % scalazV
  val scalaz_effect = scalaz %% "scalaz-effect" % scalazV
  val scalaz_concurrent = scalaz %% "scalaz-concurrent" % scalazV
  val scalacheckZ = scalaz %% "scalaz-scalacheck-binding" % scalazV % "test"

  val scalacheck = "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"

  val coolness = Seq(scalaz_core, scalaz_effect, scalaz_concurrent,
                     scalacheckZ, scalacheck)
}

object UtilBuild extends Build {
  import Dependencies._
  import BuildSettings._

  def addDeps (ds: ModuleID*) =
    BuildSettings.buildSettings ++
    Seq(libraryDependencies ++= (coolness ++ ds))

  lazy val dire = Project (
    "dire",
    file("."),
    settings = buildSettings
  ) aggregate(core, example)

  lazy val core = Project (
    "dire-core",
    file("core"),
    settings = addDeps() :+ (OsgiKeys.exportPackage := Seq("dire"))
  )
}

// vim: set ts=2 sw=2 nowrap et:
