import sbt._
import Keys._
import Dependencies._

val buildOrganization = "dire"
val buildVersion      = "0.3.0"

val buildSettings = Seq (
  organization       := buildOrganization,
  version            := buildVersion,
  scalaVersion       := sv,
  exportJars         := true,
  fork               := true,
  publishTo          := Some(Resolver.file("file", 
    new File(Path.userHome.absolutePath+"/.m2/repository"))),
  libraryDependencies in ThisBuild ++= deps,

  scalacOptions      ++= Seq(
    "-deprecation",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds"
  )
)

lazy val dire = Project("dire", file("."))
                  .settings(buildSettings)
                  .aggregate(core, swing, example)

lazy val core = Project("dire-core", file("core"))
                 .settings(buildSettings)

lazy val example = Project("dire-example", file("example"))
                     .settings(
                       buildSettings,
                       libraryDependencies += scalaXml
                     ).dependsOn(core, swing)

lazy val swing = Project("dire-swing", file("swing"))
                   .settings(buildSettings)
                   .dependsOn(core)

// vim: set ts=2 sw=2 nowrap et:
