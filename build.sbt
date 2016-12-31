val scalaTestVersion = "3.0.0-M7"
val scalinVersion = "0.11.0.4"
val shapelessVersion = "2.2.5"
val spireVersion = "0.11.0"

organization := "net.alasc"

name := "attributes"

scalaVersion := "2.11.8"

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

homepage := Some(url(s"https://github.com/denisrosset/${name.value}#readme"))

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation", "-optimize") 

libraryDependencies ++= Seq(
  "org.spire-math" %% "spire" % spireVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "com.chuusai" %% "shapeless" % shapelessVersion % "test"
)

resolvers ++= Seq(
  "bintray/denisrosset/net.alasc" at "https://dl.bintray.com/denisrosset/net.alasc",
  "bintray/non" at "http://dl.bintray.com/non/maven",
  "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/",
  Resolver.jcenterRepo
)

publishArtifact in Test := false

// This will break the process into two parts:
// First, stage all artifacts using publish.
// Once all artifacts are staged, run bintrayRelease to make the artifacts public

bintrayReleaseOnPublish in ThisBuild := false

bintrayRepository := "maven"

  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq()
      // in Scala 2.10, quasiquotes are provided by macro paradise
      case Some((2, 10)) =>
        Seq(
          compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full),
          "org.scalamacros" %% "quasiquotes" % "2.0.1" cross CrossVersion.binary
        )
    }
  }
