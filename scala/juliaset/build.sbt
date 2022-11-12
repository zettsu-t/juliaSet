lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github",
      scalaVersion := "2.13.6"
    )),
    name := "scalatest-example"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.10"
