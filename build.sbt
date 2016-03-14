val scalaTestVersion = "3.0.0-M7"
val spireVersion = "0.11.0"

name := "attributes"

organization := "net.alasc"

scalaVersion := "2.11.8"

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

homepage := Some(url(s"https://github.com/denisrosset/${name.value}#readme"))

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation", "-optimize") 

libraryDependencies ++= Seq(
  "org.spire-math" %% "spire" % spireVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
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

bintrayRepository := "net.alasc"