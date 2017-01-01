val scalaTestVersion = "3.0.1"

organization := "net.alasc"

name := "attributes"

scalaVersion := "2.11.8"

crossScalaVersions := Seq("2.10.6", "2.11.8", "2.12.1")

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

homepage := Some(url(s"https://github.com/denisrosset/${name.value}#readme"))

scalacOptions ++= commonScalacOptions

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

resolvers ++= Seq(
  "bintray/denisrosset/net.alasc" at "https://dl.bintray.com/denisrosset/net.alasc",
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

(unmanagedSourceDirectories in Compile) ++= {
  (unmanagedSourceDirectories in Compile).value.map {
    dir:File =>
    CrossVersion.partialVersion(scalaBinaryVersion.value) match {
      case Some((major, minor)) =>
        new File(s"${dir.getPath}_$major.$minor")
      case None =>
        sys.error("couldn't parse scalaBinaryVersion ${scalaBinaryVersion.value}")
    }
  }
}

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings", // removed due to inlining problems
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)
