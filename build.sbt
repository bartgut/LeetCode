name := "LeetCode"

version := "0.1"

scalaVersion := "2.13.8"

lazy val magnolia = "com.softwaremill.magnolia1_2" %% "magnolia" % "1.1.0"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"
libraryDependencies += "co.fs2" %% "fs2-core" % "2.5.10"
libraryDependencies += magnolia
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
