// project info
organization := "com.github.nmasahiro"
name := "asap"
version := "1.0.0"
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
  "org.typelevel" %% "cats-core" % "1.4.0",
  "com.typesafe" % "config" % "1.3.2",
  "org.scalactic" %% "scalactic" % "3.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)
