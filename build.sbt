ThisBuild / organization := "com.github.nmasahiro"
ThisBuild / organizationName := "nmasahiro"
ThisBuild / organizationHomepage := Some(url("https://nmasahiro.com/"))

name := "asap"

headerLicense := Some(HeaderLicense.MIT("2018", "Masahiro Nomura"))

// compile settings
scalaVersion := "2.12.7"

// disable using the Scala version in output paths and artifacts
crossPaths := false

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:experimental.macros",
  "-unchecked",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Xfuture",
  "-Ypartial-unification"
)

// dependencies
resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % "0.13.2",
  "org.scalanlp" %% "breeze-natives" % "0.13.2",
  "org.scalanlp" %% "breeze-viz" % "0.13.2",
  "com.typesafe" % "config" % "1.3.2",
  "org.scalactic" %% "scalactic" % "3.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/nmasahiro/asap"),
    "scm:git@github.com:nmasahiro/asap.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id = "nmasahiro",
    name = "Masahiro Nomura",
    email = "masahironomura5325@gmail.com",
    url = url("https://nmasahiro.com/")
  )
)

ThisBuild / description := "Evolutionary Computation Library"
ThisBuild / licenses := List("MIT" -> new URL("https://opensource.org/licenses/MIT"))
ThisBuild / homepage := Some(url("https://nmasahiro.com/"))

// remove all additional repository other than maven central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true
