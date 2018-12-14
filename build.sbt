organization := "com.github.nmasahiro"

name := "asap"

headerLicense := Some(HeaderLicense.MIT("2018", "Masahiro Nomura"))

// compile settings
scalaVersion := "2.12.7"

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

// for publishing maven
publishMavenStyle := true
publishArtifact in Test := false
pomIncludeRepository := { _ => false }
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repogitories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

scmInfo := Some(
  ScmInfo(
    url("https://github.com/nmasahiro/asap<"),
    "scm:git@github.com:nmasahiro/asap.git"
  )
)

developers := List(
  Developer(
    id = "nmasahiro",
    name = "Masahiro Nomura",
    email = "masahironomura5325@gmail.com",
    url = url("https://nmasahiro.com/")
  )
)

import ReleaseTransformations._
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  ReleaseStep(action = Command.process("publishSigned", _)),
  setNextVersion,
  commitNextVersion,
  ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
  pushChanges
)
