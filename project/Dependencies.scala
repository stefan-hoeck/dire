import sbt._

object Dependencies {
  val sv                = "2.12.3"
  val scalacheckV       = "1.12.5"
  val scalaXmlV         = "1.0.5"
  val scalaz            = "org.scalaz"
  val scalazV           = "7.2.15"

  val scalaz_core       = scalaz %% "scalaz-core" % scalazV
  val scalaz_effect     = scalaz %% "scalaz-effect" % scalazV
  val scalaz_concurrent = scalaz %% "scalaz-concurrent" % scalazV
  val scalacheckZ       = scalaz %% "scalaz-scalacheck-binding" % scalazV % "test"

  val scalaXml          = "org.scala-lang.modules" %% "scala-xml" % scalaXmlV
  val scalacheck        = "org.scalacheck" %% "scalacheck" % scalacheckV % "test"
  val scala_reflect     = "org.scala-lang" % "scala-reflect" % sv

  val deps              = Seq(scalaz_core, scalaz_effect, scalaz_concurrent,
                              scalacheckZ, scalacheck, scala_reflect)
}

// vim: set ts=2 sw=2 nowrap et:
